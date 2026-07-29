//! Built-in, native-item-only context adapters.
//!
//! These adapters never produce completion candidates. Their synchronous
//! enrichment path only inspects native shell tokens and candidate metadata;
//! external commands are reserved for delayed documentation resolution.

mod process;

use std::fmt;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use moka::sync::Cache;
use semver::Version;
use sense_model::{
    AdapterEvent, CompletionItem, CompletionKind, CompletionResource, ContextEpoch,
    DocumentationState, Enrichment, ItemCapabilities, ItemTags, MarkupContent, MarkupKind,
    RawBytes, SourceId,
};
use sense_provider_api::{
    AdapterCapabilities, AdapterContext, AdapterSelector, AdapterSink, Authority, ContextAdapter,
    ContextAdapterDescriptor, DeadlinePolicy, PROVIDER_API_VERSION, ProviderError,
};
use tokio::sync::{Semaphore, mpsc};
use tokio::task::JoinSet;
use tokio_util::sync::CancellationToken;

use crate::process::{CommandRequest, run_bounded};

const BUILTIN_MAX_ENRICHMENTS: u32 = 100_000;
const EVENT_CHANNEL_CAPACITY: usize = 32;
const DOCUMENTATION_OUTPUT_BYTES: usize = 256 * 1024;

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("adapter maximum concurrency must be greater than zero")]
    InvalidConcurrency,
    #[error("documentation resolver {0:?} must have a name, a program, and at least one kind")]
    InvalidDocumentationResolver(String),
    #[error(transparent)]
    InvalidDescriptor(#[from] ProviderError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AdapterSettings {
    pub enabled: bool,
    pub soft_timeout: Duration,
    pub hard_timeout: Duration,
}

impl AdapterSettings {
    #[must_use]
    pub const fn new(enabled: bool, soft_timeout: Duration, hard_timeout: Duration) -> Self {
        Self {
            enabled,
            soft_timeout,
            hard_timeout,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeConfig {
    pub maximum_concurrency: usize,
    pub documentation_cache_bytes: u64,
    pub documentation_cache_ttl: Duration,
    pub documentation: DocumentationAdapterSettings,
    pub git: AdapterSettings,
    pub man: AdapterSettings,
    pub systemd: AdapterSettings,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        let settings =
            AdapterSettings::new(true, Duration::from_millis(80), Duration::from_millis(500));
        Self {
            maximum_concurrency: 4,
            documentation_cache_bytes: 128 * 1024 * 1024,
            documentation_cache_ttl: Duration::from_hours(1),
            documentation: DocumentationAdapterSettings {
                settings,
                resolvers: Vec::new(),
            },
            git: settings,
            man: settings,
            systemd: settings,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentationAdapterSettings {
    pub settings: AdapterSettings,
    pub resolvers: Vec<DocumentationResolver>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentationResolver {
    pub name: String,
    pub kinds: Vec<CompletionKind>,
    pub program: RawBytes,
    pub arguments: Vec<DocumentationArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocumentationArgument {
    Literal(RawBytes),
    Value,
}

#[derive(Clone)]
pub struct AdapterRuntime {
    adapters: Vec<Arc<dyn ContextAdapter>>,
    concurrency: Arc<Semaphore>,
    documentation_cache: Option<Cache<DocumentationCacheKey, DocumentationState>>,
}

impl fmt::Debug for AdapterRuntime {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ids: Vec<&str> = self
            .adapters
            .iter()
            .map(|adapter| adapter.descriptor().id.0.as_str())
            .collect();
        formatter
            .debug_struct("AdapterRuntime")
            .field("adapters", &ids)
            .field("available_permits", &self.concurrency.available_permits())
            .field(
                "documentation_cache_enabled",
                &self.documentation_cache.is_some(),
            )
            .finish()
    }
}

impl AdapterRuntime {
    /// Construct the built-in adapter runtime after validating every bound.
    ///
    /// # Errors
    ///
    /// Returns an error for zero concurrency or an invalid adapter descriptor.
    pub fn new(config: RuntimeConfig) -> Result<Self, RuntimeError> {
        if config.maximum_concurrency == 0 {
            return Err(RuntimeError::InvalidConcurrency);
        }
        for resolver in &config.documentation.resolvers {
            if resolver.name.trim().is_empty()
                || resolver.program.is_empty()
                || resolver.kinds.is_empty()
            {
                return Err(RuntimeError::InvalidDocumentationResolver(
                    resolver.name.clone(),
                ));
            }
        }
        let mut adapters: Vec<Arc<dyn ContextAdapter>> = Vec::new();
        if config.git.enabled {
            adapters.push(Arc::new(GitAdapter::new(config.git)));
        }
        if config.systemd.enabled {
            adapters.push(Arc::new(SystemdAdapter::new(config.systemd)));
        }
        if config.documentation.settings.enabled && !config.documentation.resolvers.is_empty() {
            adapters.push(Arc::new(CommandDocumentationAdapter::new(
                config.documentation,
            )));
        }
        if config.man.enabled {
            adapters.push(Arc::new(ManAdapter::new(config.man)));
        }
        for adapter in &adapters {
            adapter.descriptor().validate()?;
        }
        let documentation_cache = (config.documentation_cache_bytes > 0
            && !config.documentation_cache_ttl.is_zero())
        .then(|| {
            Cache::builder()
                .max_capacity(config.documentation_cache_bytes)
                .time_to_live(config.documentation_cache_ttl)
                .weigher(documentation_weight)
                .build()
        });
        Ok(Self {
            adapters,
            concurrency: Arc::new(Semaphore::new(config.maximum_concurrency)),
            documentation_cache,
        })
    }

    #[must_use]
    pub fn has_resolver(&self, context: &AdapterContext, item: &CompletionItem) -> bool {
        !self
            .matching_adapters(&Operation::Resolve {
                context: context.clone(),
                item: Box::new(item.clone()),
            })
            .is_empty()
    }

    #[must_use]
    pub fn needs_enrichment(&self, context: &AdapterContext, items: &[CompletionItem]) -> bool {
        self.adapters
            .iter()
            .any(|adapter| enrichment_adapter_matches(adapter.as_ref(), context, items))
    }

    pub async fn enrich(
        &self,
        context: AdapterContext,
        items: Vec<CompletionItem>,
        cancellation: CancellationToken,
    ) -> Vec<AdapterEvent> {
        self.run(Operation::Enrich { context, items }, cancellation)
            .await
    }

    pub async fn resolve(
        &self,
        context: AdapterContext,
        item: CompletionItem,
        cancellation: CancellationToken,
    ) -> Vec<AdapterEvent> {
        let cache_key = documentation_cache_key(&context, &item);
        if let (Some(cache), Some(key)) = (&self.documentation_cache, &cache_key)
            && let Some(documentation) = cache.get(key)
        {
            return vec![AdapterEvent::Documentation {
                item_id: item.id,
                documentation,
            }];
        }
        let events = self
            .run(
                Operation::Resolve {
                    context,
                    item: Box::new(item),
                },
                cancellation,
            )
            .await;
        if let (Some(cache), Some(key), Some(documentation)) = (
            &self.documentation_cache,
            cache_key,
            resolved_documentation(&events),
        ) {
            cache.insert(key, documentation);
        }
        events
    }

    async fn run(
        &self,
        operation: Operation,
        cancellation: CancellationToken,
    ) -> Vec<AdapterEvent> {
        let matching = self.matching_adapters(&operation);
        if matching.is_empty() {
            return Vec::new();
        }

        let (sender, mut receiver) = mpsc::channel(EVENT_CHANNEL_CAPACITY);
        let mut tasks = JoinSet::new();
        for adapter in matching {
            let operation = operation.clone();
            let cancellation = cancellation.child_token();
            let sink = AdapterSink::new(sender.clone());
            let concurrency = Arc::clone(&self.concurrency);
            tasks.spawn(async move {
                let Ok(_permit) = concurrency.acquire_owned().await else {
                    return;
                };
                let hard_timeout = adapter.descriptor().deadlines.hard();
                let result = tokio::time::timeout(
                    hard_timeout,
                    operation.execute(adapter.as_ref(), &sink, &cancellation),
                )
                .await;
                match result {
                    Ok(Ok(()) | Err(ProviderError::Cancelled)) => {}
                    Ok(Err(error)) => tracing::debug!(
                        adapter = %adapter.descriptor().id.0,
                        %error,
                        "context adapter failed"
                    ),
                    Err(_) => tracing::debug!(
                        adapter = %adapter.descriptor().id.0,
                        "context adapter exceeded its hard deadline"
                    ),
                }
            });
        }
        drop(sender);

        let mut events = Vec::new();
        while let Some(event) = receiver.recv().await {
            events.push(event);
        }
        while tasks.join_next().await.is_some() {}
        events
    }

    fn matching_adapters(&self, operation: &Operation) -> Vec<Arc<dyn ContextAdapter>> {
        let context = operation.context();
        let capability = operation.capability();
        let item = operation.item();
        let matching: Vec<_> = self
            .adapters
            .iter()
            .filter(|adapter| {
                let descriptor = adapter.descriptor();
                descriptor.capabilities.contains(capability)
                    && selector_matches(descriptor, context, item)
                    && operation
                        .enrichment_items()
                        .is_none_or(|items| adapter.can_enrich(context, items))
            })
            .cloned()
            .collect();
        if capability == AdapterCapabilities::ENRICH {
            return matching;
        }
        let specialized: Vec<_> = matching
            .iter()
            .filter(|adapter| !adapter.descriptor().selectors.command_paths.is_empty())
            .cloned()
            .collect();
        if specialized.is_empty() {
            // Generic resolvers are ordered by precedence. A configured
            // resolver beats the generic man-page fallback, and only one
            // resolver may publish documentation for an item.
            matching
                .into_iter()
                .find(|adapter| adapter.descriptor().selectors.command_paths.is_empty())
                .into_iter()
                .collect()
        } else {
            specialized
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct DocumentationCacheKey {
    context_epoch: ContextEpoch,
    cwd: RawBytes,
    command_context: Vec<RawBytes>,
    label: String,
    kind: CompletionKind,
}

fn documentation_cache_key(
    context: &AdapterContext,
    item: &CompletionItem,
) -> Option<DocumentationCacheKey> {
    if !matches!(
        item.kind,
        CompletionKind::Command
            | CompletionKind::Builtin
            | CompletionKind::Subcommand
            | CompletionKind::Option
    ) {
        return None;
    }
    let current = current_word(context)?;
    let command_context = context.native_context.words.get(..current)?.to_vec();
    Some(DocumentationCacheKey {
        context_epoch: context.request.context_epoch,
        cwd: context.request.cwd.clone(),
        command_context,
        label: item.label.clone(),
        kind: item.kind,
    })
}

fn documentation_weight(_key: &DocumentationCacheKey, documentation: &DocumentationState) -> u32 {
    let bytes = match documentation {
        DocumentationState::Resolved(content) => content.value.len(),
        DocumentationState::None | DocumentationState::Unresolved => 1,
    };
    u32::try_from(bytes.max(1)).unwrap_or(u32::MAX)
}

fn resolved_documentation(events: &[AdapterEvent]) -> Option<DocumentationState> {
    events.iter().rev().find_map(|event| match event {
        AdapterEvent::Documentation { documentation, .. } => Some(documentation.clone()),
        AdapterEvent::Enrichments(_) => None,
    })
}

#[derive(Clone)]
enum Operation {
    Enrich {
        context: AdapterContext,
        items: Vec<CompletionItem>,
    },
    Resolve {
        context: AdapterContext,
        item: Box<CompletionItem>,
    },
}

impl Operation {
    const fn context(&self) -> &AdapterContext {
        match self {
            Self::Enrich { context, .. } | Self::Resolve { context, .. } => context,
        }
    }

    const fn capability(&self) -> AdapterCapabilities {
        match self {
            Self::Enrich { .. } => AdapterCapabilities::ENRICH,
            Self::Resolve { .. } => AdapterCapabilities::RESOLVE,
        }
    }

    fn item(&self) -> Option<&CompletionItem> {
        match self {
            Self::Enrich { .. } => None,
            Self::Resolve { item, .. } => Some(item),
        }
    }

    fn enrichment_items(&self) -> Option<&[CompletionItem]> {
        match self {
            Self::Enrich { items, .. } => Some(items),
            Self::Resolve { .. } => None,
        }
    }

    async fn execute(
        &self,
        adapter: &dyn ContextAdapter,
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        match self {
            Self::Enrich { context, items } => {
                adapter.enrich(context, items, sink, cancellation).await
            }
            Self::Resolve { context, item } => {
                adapter
                    .resolve(context, item.as_ref(), sink, cancellation)
                    .await
            }
        }
    }
}

fn enrichment_adapter_matches(
    adapter: &dyn ContextAdapter,
    context: &AdapterContext,
    items: &[CompletionItem],
) -> bool {
    let descriptor = adapter.descriptor();
    descriptor
        .capabilities
        .contains(AdapterCapabilities::ENRICH)
        && selector_matches(descriptor, context, None)
        && adapter.can_enrich(context, items)
}

fn selector_matches(
    descriptor: &ContextAdapterDescriptor,
    context: &AdapterContext,
    item: Option<&CompletionItem>,
) -> bool {
    let command_matches = descriptor.selectors.command_paths.is_empty()
        || descriptor.selectors.command_paths.iter().any(|path| {
            path.len() <= context.native_context.words.len()
                && path
                    .iter()
                    .zip(&context.native_context.words)
                    .all(|(expected, actual)| expected.as_bytes() == actual.as_slice())
        });
    let context_matches = descriptor.selectors.contexts.is_empty()
        || item.is_none_or(|item| {
            descriptor
                .selectors
                .contexts
                .iter()
                .any(|context| context == completion_kind_name(item.kind))
        });
    command_matches && context_matches
}

struct GitAdapter {
    descriptor: ContextAdapterDescriptor,
}

impl GitAdapter {
    fn new(settings: AdapterSettings) -> Self {
        Self {
            descriptor: descriptor(
                "git",
                "Git",
                Some("git"),
                &[
                    CompletionKind::Option,
                    CompletionKind::Subcommand,
                    CompletionKind::GitBranch,
                    CompletionKind::GitTag,
                    CompletionKind::GitCommit,
                ],
                settings,
                AdapterCapabilities::ENRICH | AdapterCapabilities::RESOLVE,
                true,
            ),
        }
    }
}

#[async_trait]
impl ContextAdapter for GitAdapter {
    fn descriptor(&self) -> &ContextAdapterDescriptor {
        &self.descriptor
    }

    async fn enrich(
        &self,
        context: &AdapterContext,
        items: &[CompletionItem],
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let Some(current_word) = current_word(context) else {
            return Ok(());
        };
        let enrichments: Vec<_> = items
            .iter()
            .filter_map(|item| git_enrichment(current_word, item))
            .collect();
        if enrichments.is_empty() {
            return Ok(());
        }
        sink.send(AdapterEvent::Enrichments(enrichments), cancellation)
            .await
    }

    async fn resolve(
        &self,
        context: &AdapterContext,
        item: &CompletionItem,
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let request = git_documentation_request(context, item)?;
        let output = run_bounded(request, cancellation).await?;
        let Some(output) = focus_documentation(output, item) else {
            return Ok(());
        };
        publish_documentation(item, output, sink, cancellation).await
    }
}

struct ManAdapter {
    descriptor: ContextAdapterDescriptor,
}

impl ManAdapter {
    fn new(settings: AdapterSettings) -> Self {
        Self {
            descriptor: descriptor(
                "man",
                "manual pages",
                None,
                &[CompletionKind::Option],
                settings,
                AdapterCapabilities::RESOLVE,
                true,
            ),
        }
    }
}

#[async_trait]
impl ContextAdapter for ManAdapter {
    fn descriptor(&self) -> &ContextAdapterDescriptor {
        &self.descriptor
    }

    async fn resolve(
        &self,
        context: &AdapterContext,
        item: &CompletionItem,
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let request = man_documentation_request(context, item)?;
        let output = run_bounded(request, cancellation).await?;
        let Some(output) = focus_documentation(output, item) else {
            return Ok(());
        };
        publish_documentation(item, output, sink, cancellation).await
    }
}

struct SystemdAdapter {
    descriptor: ContextAdapterDescriptor,
}

impl SystemdAdapter {
    fn new(settings: AdapterSettings) -> Self {
        Self {
            descriptor: descriptor(
                "systemd",
                "systemd",
                Some("systemctl"),
                &[
                    CompletionKind::Option,
                    CompletionKind::Subcommand,
                    CompletionKind::Service,
                ],
                settings,
                AdapterCapabilities::ENRICH | AdapterCapabilities::RESOLVE,
                true,
            ),
        }
    }
}

#[async_trait]
impl ContextAdapter for SystemdAdapter {
    fn descriptor(&self) -> &ContextAdapterDescriptor {
        &self.descriptor
    }

    async fn enrich(
        &self,
        context: &AdapterContext,
        items: &[CompletionItem],
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let Some(current_word) = current_word(context) else {
            return Ok(());
        };
        let enrichments: Vec<_> = items
            .iter()
            .map(|item| systemd_enrichment(context, current_word, item))
            .collect();
        if enrichments.is_empty() {
            return Ok(());
        }
        sink.send(AdapterEvent::Enrichments(enrichments), cancellation)
            .await
    }

    async fn resolve(
        &self,
        context: &AdapterContext,
        item: &CompletionItem,
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let request = systemd_documentation_request(context, item)?;
        let output = run_bounded(request, cancellation).await?;
        let Some(output) = focus_documentation(output, item) else {
            return Ok(());
        };
        publish_documentation(item, output, sink, cancellation).await
    }
}

struct CommandDocumentationAdapter {
    descriptor: ContextAdapterDescriptor,
    resolvers: Vec<DocumentationResolver>,
}

impl CommandDocumentationAdapter {
    fn new(config: DocumentationAdapterSettings) -> Self {
        let mut kinds = Vec::new();
        for kind in config
            .resolvers
            .iter()
            .flat_map(|resolver| resolver.kinds.iter().copied())
        {
            if !kinds.contains(&kind) {
                kinds.push(kind);
            }
        }
        Self {
            descriptor: descriptor(
                "configured-documentation",
                "configured documentation",
                None,
                &kinds,
                config.settings,
                AdapterCapabilities::ENRICH | AdapterCapabilities::RESOLVE,
                false,
            ),
            resolvers: config.resolvers,
        }
    }

    fn resolver_for(&self, item: &CompletionItem) -> Option<&DocumentationResolver> {
        self.resolvers
            .iter()
            .find(|resolver| resolver_supports_item(resolver, item))
    }
}

#[async_trait]
impl ContextAdapter for CommandDocumentationAdapter {
    fn descriptor(&self) -> &ContextAdapterDescriptor {
        &self.descriptor
    }

    fn can_enrich(&self, _context: &AdapterContext, items: &[CompletionItem]) -> bool {
        items.iter().any(|item| self.resolver_for(item).is_some())
    }

    async fn enrich(
        &self,
        _context: &AdapterContext,
        items: &[CompletionItem],
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let enrichments: Vec<_> = items
            .iter()
            .filter(|item| self.resolver_for(item).is_some())
            .map(|item| Enrichment {
                item_id: item.id.clone(),
                kind: None,
                add_tags: ItemTags::empty(),
                add_capabilities: ItemCapabilities::RESOLVE_DOCUMENTATION,
                detail: None,
                documentation: None,
            })
            .collect();
        if enrichments.is_empty() {
            return Ok(());
        }
        sink.send(AdapterEvent::Enrichments(enrichments), cancellation)
            .await
    }

    async fn resolve(
        &self,
        context: &AdapterContext,
        item: &CompletionItem,
        sink: &AdapterSink,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        let Some(resolver) = self.resolver_for(item) else {
            return Ok(());
        };
        let Some(arguments) = resolver
            .arguments
            .iter()
            .map(|argument| documentation_argument(argument, item))
            .collect::<Option<Vec<_>>>()
        else {
            return Ok(());
        };
        let output = run_bounded(
            CommandRequest {
                program: resolver.program.clone(),
                arguments,
                cwd: context.request.cwd.clone(),
                timeout: self.descriptor.deadlines.hard(),
                maximum_output_bytes: DOCUMENTATION_OUTPUT_BYTES,
            },
            cancellation,
        )
        .await?;
        publish_documentation(item, output, sink, cancellation).await
    }
}

fn resolver_supports_item(resolver: &DocumentationResolver, item: &CompletionItem) -> bool {
    resolver.kinds.contains(&item.kind)
        && (!resolver.arguments.contains(&DocumentationArgument::Value)
            || documentation_value(item).is_some())
}

fn documentation_argument(
    argument: &DocumentationArgument,
    item: &CompletionItem,
) -> Option<RawBytes> {
    match argument {
        DocumentationArgument::Literal(value) => Some(value.clone()),
        DocumentationArgument::Value => documentation_value(item),
    }
}

fn documentation_value(item: &CompletionItem) -> Option<RawBytes> {
    if matches!(
        item.kind,
        CompletionKind::File | CompletionKind::Directory | CompletionKind::Symlink
    ) {
        filesystem_path(item).cloned()
    } else {
        Some(RawBytes::from(item.label.as_str()))
    }
}

fn filesystem_path(item: &CompletionItem) -> Option<&RawBytes> {
    match item.resource.as_ref()? {
        CompletionResource::FileSystemPath { path } => Some(path),
    }
}

fn descriptor(
    id: &str,
    display_name: &str,
    command: Option<&str>,
    contexts: &[CompletionKind],
    settings: AdapterSettings,
    capabilities: AdapterCapabilities,
    side_effect_free: bool,
) -> ContextAdapterDescriptor {
    ContextAdapterDescriptor {
        id: SourceId(id.into()),
        display_name: display_name.into(),
        version: Version::new(0, 1, 0),
        api_version: PROVIDER_API_VERSION,
        capabilities,
        authority: Authority::Authoritative,
        selectors: AdapterSelector {
            command_paths: command
                .map(|command| vec![vec![command.into()]])
                .unwrap_or_default(),
            contexts: contexts
                .iter()
                .map(|kind| completion_kind_name(*kind).into())
                .collect(),
        },
        deadlines: DeadlinePolicy {
            soft_ms: duration_ms(settings.soft_timeout),
            hard_ms: duration_ms(settings.hard_timeout),
        },
        maximum_concurrency: 1,
        maximum_enrichments: BUILTIN_MAX_ENRICHMENTS,
        cancellation: true,
        side_effect_free,
    }
}

const fn completion_kind_name(kind: CompletionKind) -> &'static str {
    match kind {
        CompletionKind::Text => "text",
        CompletionKind::Command => "command",
        CompletionKind::Alias => "alias",
        CompletionKind::Builtin => "builtin",
        CompletionKind::Function => "function",
        CompletionKind::Subcommand => "subcommand",
        CompletionKind::Option => "option",
        CompletionKind::OptionValue => "option-value",
        CompletionKind::Variable => "variable",
        CompletionKind::File => "file",
        CompletionKind::Directory => "directory",
        CompletionKind::Symlink => "symlink",
        CompletionKind::User => "user",
        CompletionKind::Host => "host",
        CompletionKind::Process => "process",
        CompletionKind::Job => "job",
        CompletionKind::GitBranch => "git-branch",
        CompletionKind::GitTag => "git-tag",
        CompletionKind::GitCommit => "git-commit",
        CompletionKind::Service => "service",
        CompletionKind::Container => "container",
        CompletionKind::Image => "image",
        CompletionKind::Package => "package",
    }
}

fn duration_ms(duration: Duration) -> u64 {
    u64::try_from(duration.as_millis()).unwrap_or(u64::MAX)
}

fn current_word(context: &AdapterContext) -> Option<usize> {
    context
        .native_context
        .current_word
        .and_then(|index| usize::try_from(index).ok())
}

fn git_enrichment(current_word: usize, item: &CompletionItem) -> Option<Enrichment> {
    let kind = if item.label.starts_with('-') {
        Some(CompletionKind::Option)
    } else if current_word == 1 {
        Some(CompletionKind::Subcommand)
    } else {
        git_kind_from_native_group(item)
    };
    let resolvable = kind.is_some_and(|kind| {
        matches!(
            kind,
            CompletionKind::Option
                | CompletionKind::Subcommand
                | CompletionKind::GitBranch
                | CompletionKind::GitTag
                | CompletionKind::GitCommit
        )
    });
    (kind.is_some() || resolvable).then(|| Enrichment {
        item_id: item.id.clone(),
        kind,
        add_tags: ItemTags::empty(),
        add_capabilities: if resolvable {
            ItemCapabilities::RESOLVE_DOCUMENTATION
        } else {
            ItemCapabilities::empty()
        },
        detail: None,
        documentation: None,
    })
}

fn git_kind_from_native_group(item: &CompletionItem) -> Option<CompletionKind> {
    let group = item.group.as_ref()?.0.to_ascii_lowercase();
    if group.contains("branch") || group.contains("head") || group.contains("remote") {
        Some(CompletionKind::GitBranch)
    } else if group.contains("tag") {
        Some(CompletionKind::GitTag)
    } else if group.contains("commit") || group.contains("revision") {
        Some(CompletionKind::GitCommit)
    } else {
        None
    }
}

fn systemd_enrichment(
    context: &AdapterContext,
    current_word: usize,
    item: &CompletionItem,
) -> Enrichment {
    let kind = if item.label.starts_with('-') {
        CompletionKind::Option
    } else if current_word == 1 {
        CompletionKind::Subcommand
    } else if unit_argument_context(context, current_word) {
        CompletionKind::Service
    } else {
        item.kind
    };
    let resolvable = matches!(
        kind,
        CompletionKind::Option | CompletionKind::Subcommand | CompletionKind::Service
    );
    Enrichment {
        item_id: item.id.clone(),
        kind: Some(kind),
        add_tags: ItemTags::empty(),
        add_capabilities: if resolvable {
            ItemCapabilities::RESOLVE_DOCUMENTATION
        } else {
            ItemCapabilities::empty()
        },
        detail: None,
        documentation: None,
    }
}

fn unit_argument_context(context: &AdapterContext, current_word: usize) -> bool {
    const UNIT_COMMANDS: &[&[u8]] = &[
        b"cat",
        b"disable",
        b"edit",
        b"enable",
        b"is-active",
        b"is-enabled",
        b"mask",
        b"reload-or-restart",
        b"restart",
        b"show",
        b"start",
        b"status",
        b"stop",
        b"try-restart",
        b"unmask",
    ];
    current_word >= 2
        && context.native_context.words.get(1).is_some_and(|word| {
            UNIT_COMMANDS
                .iter()
                .any(|command| *command == word.as_slice())
        })
}

fn git_documentation_request(
    context: &AdapterContext,
    item: &CompletionItem,
) -> Result<CommandRequest, ProviderError> {
    let current_word = current_word(context).ok_or_else(|| {
        ProviderError::Failed("Git completion context has no current word".into())
    })?;
    let mut arguments = Vec::new();
    if current_word == 1 && !item.label.starts_with('-') {
        arguments.extend([RawBytes::from(item.label.as_str()), RawBytes::from("-h")]);
    } else if matches!(
        item.kind,
        CompletionKind::GitBranch | CompletionKind::GitTag | CompletionKind::GitCommit
    ) {
        arguments.extend([
            RawBytes::from("log"),
            RawBytes::from("-1"),
            RawBytes::from("--format=%D%n%h  %s%nAuthor: %an%nDate: %ad"),
            RawBytes::from("--date=relative"),
            RawBytes::from("--end-of-options"),
            RawBytes::from(item.label.as_str()),
        ]);
    } else {
        if let Some(subcommand) = context.native_context.words.get(1) {
            arguments.push(subcommand.clone());
        }
        arguments.push(RawBytes::from("-h"));
    }
    Ok(CommandRequest {
        program: RawBytes::from("git"),
        arguments,
        cwd: context.request.cwd.clone(),
        timeout: Duration::from_millis(450),
        maximum_output_bytes: DOCUMENTATION_OUTPUT_BYTES,
    })
}

fn man_documentation_request(
    context: &AdapterContext,
    item: &CompletionItem,
) -> Result<CommandRequest, ProviderError> {
    let current_word = current_word(context).ok_or_else(|| {
        ProviderError::Failed("manual-page completion context has no current word".into())
    })?;
    let page = if current_word == 0 {
        RawBytes::from(item.label.as_str())
    } else {
        context
            .native_context
            .words
            .first()
            .cloned()
            .ok_or_else(|| ProviderError::Failed("manual page has no command word".into()))?
    };
    Ok(CommandRequest {
        program: RawBytes::from("man"),
        arguments: vec![
            RawBytes::from("--pager=cat"),
            RawBytes::from("--no-hyphenation"),
            RawBytes::from("--no-justification"),
            RawBytes::from("--"),
            page,
        ],
        cwd: context.request.cwd.clone(),
        timeout: Duration::from_millis(450),
        maximum_output_bytes: DOCUMENTATION_OUTPUT_BYTES,
    })
}

fn systemd_documentation_request(
    context: &AdapterContext,
    item: &CompletionItem,
) -> Result<CommandRequest, ProviderError> {
    let current_word = current_word(context).ok_or_else(|| {
        ProviderError::Failed("systemd completion context has no current word".into())
    })?;
    let arguments = if current_word >= 2 && unit_argument_context(context, current_word) {
        vec![
            RawBytes::from("show"),
            RawBytes::from("--no-pager"),
            RawBytes::from(
                "--property=Id,Description,LoadState,ActiveState,SubState,UnitFileState",
            ),
            RawBytes::from("--"),
            RawBytes::from(item.label.as_str()),
        ]
    } else {
        vec![RawBytes::from("--help")]
    };
    Ok(CommandRequest {
        program: RawBytes::from("systemctl"),
        arguments,
        cwd: context.request.cwd.clone(),
        timeout: Duration::from_millis(450),
        maximum_output_bytes: DOCUMENTATION_OUTPUT_BYTES,
    })
}

async fn publish_documentation(
    item: &CompletionItem,
    output: String,
    sink: &AdapterSink,
    cancellation: &CancellationToken,
) -> Result<(), ProviderError> {
    if output.trim().is_empty() {
        return Ok(());
    }
    sink.send(
        AdapterEvent::Documentation {
            item_id: item.id.clone(),
            documentation: DocumentationState::Resolved(MarkupContent {
                kind: MarkupKind::PlainText,
                value: output,
            }),
        },
        cancellation,
    )
    .await
}

fn focus_documentation(output: String, item: &CompletionItem) -> Option<String> {
    if matches!(
        item.kind,
        CompletionKind::Option | CompletionKind::Subcommand
    ) {
        extract_help_entry(&output, &item.label)
    } else {
        Some(output)
    }
}

fn extract_help_entry(output: &str, label: &str) -> Option<String> {
    let lines: Vec<_> = output.lines().collect();
    let option = option_name(label).is_some();
    let start = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| line_declares_label(line, label))
        .min_by_key(|(_, line)| indentation(line))
        .map(|(index, _)| index)?;
    let base_indent = indentation(lines[start]);
    let mut end = start + 1;
    while let Some(line) = lines.get(end) {
        if line.trim().is_empty() {
            break;
        }
        if indentation(line) <= base_indent && starts_help_entry(line, option) {
            break;
        }
        end += 1;
    }
    Some(lines[start..end].join("\n"))
}

fn line_declares_label(line: &str, label: &str) -> bool {
    if let Some(label) = option_name(label) {
        return declared_options(line).contains(&label);
    }
    line.split_whitespace()
        .next()
        .is_some_and(|word| word.trim_end_matches([':', ',']) == label)
}

fn declared_options(line: &str) -> Vec<&str> {
    let line = line.trim_start();
    let declaration = declaration_prefix(line);
    let words: Vec<_> = declaration.split_whitespace().collect();
    let mut options = Vec::new();
    for (index, word) in words.iter().enumerate() {
        let word = word.trim_matches(|character| matches!(character, '(' | ')' | ',' | '|'));
        if let Some(option) = option_name(word) {
            options.push(option);
            continue;
        }
        let followed_by_alias = words
            .get(index + 1)
            .and_then(|next| option_name(next.trim_matches([',', '|'])))
            .is_some();
        if !(looks_like_option_argument(word) || word.ends_with(',') && followed_by_alias) {
            break;
        }
    }
    options
}

fn looks_like_option_argument(word: &str) -> bool {
    let word = word.trim_matches(|character: char| !character.is_ascii_alphanumeric());
    !word.is_empty()
        && word
            .chars()
            .all(|character| character.is_ascii_uppercase() || character.is_ascii_digit())
}

fn declaration_prefix(line: &str) -> &str {
    let mut whitespace_start = None;
    let mut whitespace_count = 0;
    for (index, character) in line.char_indices() {
        if character == '\t' {
            return &line[..index];
        }
        if character.is_whitespace() {
            whitespace_start.get_or_insert(index);
            whitespace_count += 1;
            if whitespace_count == 2 {
                return &line[..whitespace_start.unwrap_or(index)];
            }
        } else {
            whitespace_start = None;
            whitespace_count = 0;
        }
    }
    line
}

fn option_name(value: &str) -> Option<&str> {
    let value = value.trim_start_matches(['[', '(']);
    let hyphens = value.bytes().take_while(|byte| *byte == b'-').count();
    if !(1..=2).contains(&hyphens) {
        return None;
    }
    let name_bytes = value[hyphens..]
        .bytes()
        .take_while(|byte| byte.is_ascii_alphanumeric() || matches!(*byte, b'-' | b'?' | b'_'))
        .count();
    (name_bytes > 0).then_some(&value[..hyphens + name_bytes])
}

fn indentation(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

fn starts_help_entry(line: &str, option: bool) -> bool {
    let first = line.split_whitespace().next().unwrap_or("");
    if option {
        option_name(first).is_some()
    } else {
        first
            .bytes()
            .next()
            .is_some_and(|byte| byte.is_ascii_alphanumeric())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use sense_model::{
        ByteOffset, CompletionRequest, ContextEpoch, Generation, NativeCommandContext, NativeShell,
        RequestId, SessionId, TerminalDimensions, TextEdit, TextRange, TriggerKind,
    };

    use super::*;

    fn context(words: &[&str], current_word: u32) -> AdapterContext {
        AdapterContext {
            request: CompletionRequest {
                session_id: SessionId::new(),
                request_id: RequestId(1),
                generation: Generation(1),
                context_epoch: ContextEpoch::default(),
                buffer: RawBytes::default(),
                cursor: ByteOffset(0),
                cwd: RawBytes::from("/tmp"),
                keymap: "default".into(),
                terminal: TerminalDimensions::default(),
                trigger: TriggerKind::Automatic,
                environment: BTreeMap::new(),
            },
            native_context: NativeCommandContext {
                words: words.iter().copied().map(RawBytes::from).collect(),
                current_word: Some(current_word),
            },
        }
    }

    fn item(label: &str) -> CompletionItem {
        CompletionItem::native(
            label,
            NativeShell::Zsh,
            label,
            TextEdit::new(TextRange::new(0, 0), label),
            label,
        )
    }

    fn path_item(label: &str, kind: CompletionKind, path: &[u8]) -> CompletionItem {
        let mut item = item(label);
        item.kind = kind;
        item.resource = Some(CompletionResource::FileSystemPath {
            path: RawBytes::from(path),
        });
        item
    }

    fn path_resolver_runtime() -> AdapterRuntime {
        let mut config = RuntimeConfig::default();
        config.documentation.resolvers = vec![DocumentationResolver {
            name: "path-metadata".into(),
            kinds: vec![CompletionKind::File, CompletionKind::Directory],
            program: RawBytes::from("file"),
            arguments: vec![
                DocumentationArgument::Literal(RawBytes::from("--")),
                DocumentationArgument::Value,
            ],
        }];
        AdapterRuntime::new(config).unwrap()
    }

    #[tokio::test]
    async fn git_subcommands_are_tagged_without_adding_items() {
        let runtime = AdapterRuntime::new(RuntimeConfig::default()).unwrap();
        let items = vec![item("checkout"), item("cherry-pick")];
        let events = runtime
            .enrich(context(&["git", "che"], 1), items, CancellationToken::new())
            .await;
        let [AdapterEvent::Enrichments(enrichments)] = events.as_slice() else {
            panic!("expected one enrichment event");
        };
        assert_eq!(enrichments.len(), 2);
        assert!(enrichments.iter().all(|enrichment| {
            enrichment.kind == Some(CompletionKind::Subcommand)
                && enrichment
                    .add_capabilities
                    .contains(ItemCapabilities::RESOLVE_DOCUMENTATION)
        }));
    }

    #[tokio::test]
    async fn systemctl_unit_arguments_are_services() {
        let runtime = AdapterRuntime::new(RuntimeConfig::default()).unwrap();
        let events = runtime
            .enrich(
                context(&["systemctl", "restart", "ng"], 2),
                vec![item("nginx.service")],
                CancellationToken::new(),
            )
            .await;
        let [AdapterEvent::Enrichments(enrichments)] = events.as_slice() else {
            panic!("expected one enrichment event");
        };
        assert_eq!(enrichments[0].kind, Some(CompletionKind::Service));
    }

    #[tokio::test]
    async fn configured_path_resolver_enriches_only_items_with_typed_paths() {
        let runtime = path_resolver_runtime();
        let adapter_context = context(&["ls", ""], 1);
        let with_path = path_item(
            "name with spaces",
            CompletionKind::File,
            b"/tmp/name with spaces;still-one-argument",
        );
        let mut without_path = item("presentation-only");
        without_path.kind = CompletionKind::File;
        assert!(
            runtime.needs_enrichment(&adapter_context, &[with_path.clone(), without_path.clone()])
        );
        assert!(!runtime.needs_enrichment(&adapter_context, &[without_path.clone()]));
        let events = runtime
            .enrich(
                adapter_context,
                vec![with_path.clone(), without_path],
                CancellationToken::new(),
            )
            .await;
        let [AdapterEvent::Enrichments(enrichments)] = events.as_slice() else {
            panic!("expected one enrichment event");
        };
        assert_eq!(enrichments.len(), 1);
        assert_eq!(enrichments[0].item_id, with_path.id);
        assert!(
            enrichments[0]
                .add_capabilities
                .contains(ItemCapabilities::RESOLVE_DOCUMENTATION)
        );
    }

    #[test]
    fn documentation_value_preserves_path_bytes_as_one_argument() {
        let path = b"/tmp/name with spaces;$(not-a-shell)";
        let item = path_item("display", CompletionKind::File, path);
        assert_eq!(
            documentation_argument(&DocumentationArgument::Value, &item)
                .unwrap()
                .as_slice(),
            path
        );
    }

    #[test]
    fn configured_resolver_precedes_the_generic_man_fallback() {
        let runtime = path_resolver_runtime();
        let item = path_item("notes", CompletionKind::File, b"/tmp/notes");
        let operation = Operation::Resolve {
            context: context(&["ls", ""], 1),
            item: Box::new(item),
        };
        let matching = runtime.matching_adapters(&operation);
        assert_eq!(matching.len(), 1);
        assert_eq!(matching[0].descriptor().id.0, "configured-documentation");
    }

    #[test]
    fn man_is_a_resolve_only_fallback_for_other_commands() {
        let runtime = AdapterRuntime::new(RuntimeConfig::default()).unwrap();
        let context = context(&["ls", "--a"], 1);
        let mut option = item("--all");
        option.kind = CompletionKind::Option;
        assert!(runtime.has_resolver(&context, &option));
        assert!(!runtime.needs_enrichment(&context, &[]));
    }

    #[test]
    fn documentation_cache_ignores_the_transient_fuzzy_fragment() {
        let mut candidate = item("--all");
        candidate.kind = CompletionKind::Option;
        let short = documentation_cache_key(&context(&["ls", "-a"], 1), &candidate).unwrap();
        let long = documentation_cache_key(&context(&["ls", "--al"], 1), &candidate).unwrap();
        assert!(short == long);
    }

    #[test]
    fn focused_help_keeps_the_selected_entry_and_its_continuation() {
        let help = "Commands:\n  reload PATTERN...\n      Reload matching units\n  restart PATTERN...\n      Stop and then start matching units\n  status PATTERN...\n      Show runtime status\n";
        assert_eq!(
            extract_help_entry(help, "restart").as_deref(),
            Some("  restart PATTERN...\n      Stop and then start matching units")
        );
    }

    #[test]
    fn focused_help_matches_long_options_at_token_boundaries() {
        let help = "  -a, --all       Show all units\n  -q, --quiet     Suppress output\n";
        assert_eq!(
            extract_help_entry(help, "--all").as_deref(),
            Some("  -a, --all       Show all units")
        );
    }

    #[test]
    fn focused_help_ignores_option_mentions_in_another_entry() {
        let help = "     --sort=WORD\n            change default 'name' sort to WORD: none (-U), size (-S), time (-t),\n            version (-v), extension (-X), width (none)\n\n     -v     natural sort of (version) numbers within text\n\n     -w, --width=COLS\n            set output width to COLS\n";
        assert_eq!(
            extract_help_entry(help, "-v").as_deref(),
            Some("     -v     natural sort of (version) numbers within text")
        );
    }

    #[test]
    fn focused_help_does_not_treat_prose_as_an_option_declaration() {
        let help = "  --sort=WORD\n      sort by WORD, including version (-v)\n";
        assert_eq!(extract_help_entry(help, "-v"), None);
    }

    #[test]
    fn focused_help_matches_an_option_with_a_separate_argument() {
        let help = "  -j N, --jobs N  Run N jobs in parallel\n  -q, --quiet      Suppress output\n";
        assert_eq!(
            extract_help_entry(help, "--jobs").as_deref(),
            Some("  -j N, --jobs N  Run N jobs in parallel")
        );
    }
}
