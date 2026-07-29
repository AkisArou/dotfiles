//! Contracts for native shell completion providers and context adapters.
//!
//! Candidate authority belongs exclusively to the active shell's native
//! completion provider. Context adapters may enrich or resolve those items,
//! but cannot create completion candidates.

use std::time::Duration;

use async_trait::async_trait;
use bitflags::bitflags;
use semver::Version;
use sense_model::{
    AdapterEvent, ByteOffset, CompletionItem, CompletionKind, CompletionRequest,
    CompletionResource, Confidence, DocumentationState, GroupId, InsertStrategy, ItemCapabilities,
    ItemId, ItemTags, MarkupContent, NativeCommandContext, NativeShell, RawBytes, SourceId,
    TextEdit, TextRange,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

pub const PROVIDER_API_VERSION: u16 = 4;

bitflags! {
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct NativeProviderCapabilities: u16 {
        const DESCRIPTIONS = 1 << 0;
        const GROUPS = 1 << 1;
        const KINDS = 1 << 2;
        const BROAD_QUERY = 1 << 3;
        const DOCUMENTATION = 1 << 4;
        const PARTIAL_ACCEPT = 1 << 5;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct AdapterCapabilities: u16 {
        const ENRICH = 1 << 0;
        const RESOLVE = 1 << 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Authority {
    Advisory,
    Inferred,
    Partial,
    Authoritative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct DeadlinePolicy {
    pub soft_ms: u64,
    pub hard_ms: u64,
}

impl DeadlinePolicy {
    #[must_use]
    pub const fn soft(self) -> Duration {
        Duration::from_millis(self.soft_ms)
    }

    #[must_use]
    pub const fn hard(self) -> Duration {
        Duration::from_millis(self.hard_ms)
    }

    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.soft_ms <= self.hard_ms && self.hard_ms > 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum NativeQueryMode {
    Exact,
    Broad,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeQuery {
    pub mode: NativeQueryMode,
    pub buffer: RawBytes,
    pub cursor: ByteOffset,
    /// Token range in the original command buffer.
    pub original_token: TextRange,
    /// Corresponding range in `buffer` after query broadening.
    pub query_token: TextRange,
    /// Structural token prefix retained for the native provider, such as
    /// `--`, `dotfiles/`, or `$`.
    pub retained_prefix: RawBytes,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeProviderDescriptor {
    pub shell: NativeShell,
    pub version: Version,
    pub api_version: u16,
    pub capabilities: NativeProviderCapabilities,
    pub deadlines: DeadlinePolicy,
    pub maximum_candidates: u32,
    pub cancellation: bool,
}

impl NativeProviderDescriptor {
    /// Validate API compatibility and resource bounds.
    ///
    /// # Errors
    ///
    /// Returns an error for incompatible APIs, invalid deadlines, or an
    /// unbounded candidate declaration.
    pub fn validate(&self) -> Result<(), ProviderError> {
        validate_api(self.api_version, self.shell.source_name())?;
        if !self.deadlines.is_valid() {
            return Err(ProviderError::InvalidDescriptor(format!(
                "{} has invalid deadlines",
                self.shell.source_name()
            )));
        }
        if self.maximum_candidates == 0 {
            return Err(ProviderError::InvalidDescriptor(format!(
                "{} must allow at least one candidate",
                self.shell.source_name()
            )));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeCompletionContext {
    pub request: CompletionRequest,
    pub query: NativeQuery,
}

/// Construct an exact native query from a token range reported by the shell.
///
/// # Errors
///
/// Returns an error if the range or cursor is invalid for the request buffer.
pub fn exact_query(
    request: &CompletionRequest,
    current_token: TextRange,
) -> Result<NativeQuery, ProviderError> {
    validate_query_range(request, current_token)?;
    Ok(NativeQuery {
        mode: NativeQueryMode::Exact,
        buffer: request.buffer.clone(),
        cursor: request.cursor,
        original_token: current_token,
        query_token: current_token,
        retained_prefix: RawBytes::default(),
    })
}

/// Remove only the fuzzy fragment while retaining shell structure.
///
/// Native completion remains the sole candidate authority: this function
/// broadens the text shown to that same provider and never manufactures an
/// item itself.
///
/// # Errors
///
/// Returns an error if the range or cursor is invalid for the request buffer.
pub fn broad_query(
    request: &CompletionRequest,
    current_token: TextRange,
) -> Result<NativeQuery, ProviderError> {
    validate_query_range(request, current_token)?;
    let token_start = current_token.start.as_usize();
    let cursor = request.cursor.as_usize();
    let token_prefix = &request.buffer.as_slice()[token_start..cursor];
    let retained_len = structural_prefix_len(token_prefix);
    let retained_prefix = RawBytes::from(&token_prefix[..retained_len]);

    let mut buffer = Vec::with_capacity(
        request.buffer.len() - current_token.end.as_usize() + token_start + retained_len,
    );
    buffer.extend_from_slice(&request.buffer.as_slice()[..token_start]);
    buffer.extend_from_slice(retained_prefix.as_slice());
    buffer.extend_from_slice(&request.buffer.as_slice()[current_token.end.as_usize()..]);

    let query_end = token_start + retained_len;
    let query_end = u32::try_from(query_end)
        .map_err(|_| ProviderError::InvalidQuery("query offset exceeds u32".into()))?;
    Ok(NativeQuery {
        mode: NativeQueryMode::Broad,
        buffer: RawBytes::from(buffer),
        cursor: ByteOffset(query_end),
        original_token: current_token,
        query_token: TextRange::new(current_token.start.0, query_end),
        retained_prefix,
    })
}

fn validate_query_range(
    request: &CompletionRequest,
    current_token: TextRange,
) -> Result<(), ProviderError> {
    if !request.cursor_is_valid()
        || !current_token.is_valid_for(request.buffer.as_slice())
        || request.cursor < current_token.start
        || request.cursor > current_token.end
    {
        return Err(ProviderError::InvalidQuery(
            "current token does not contain the request cursor".into(),
        ));
    }
    Ok(())
}

fn structural_prefix_len(token: &[u8]) -> usize {
    if let Some(index) = token.iter().rposition(|byte| *byte == b'/') {
        return index + 1;
    }
    if let Some(index) = token.iter().rposition(|byte| *byte == b'=') {
        return index + 1;
    }

    let quote_len = usize::from(matches!(token.first(), Some(b'\'' | b'"')));
    let unquoted = &token[quote_len..];
    quote_len
        + if unquoted.starts_with(b"--") {
            2
        } else {
            usize::from(matches!(unquoted.first(), Some(b'-' | b'+' | b'$' | b'~')))
        }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeCandidate {
    pub id: ItemId,
    pub label: String,
    pub label_detail: Option<String>,
    pub filter_text: Option<String>,
    pub kind: CompletionKind,
    pub tags: ItemTags,
    pub detail: Option<String>,
    pub documentation: DocumentationState,
    pub group: Option<GroupId>,
    pub edit: TextEdit,
    /// Opaque, request-scoped identity replayed only by the owning shell.
    pub acceptance: RawBytes,
    pub original_order: u32,
    pub confidence: Confidence,
    pub capabilities: ItemCapabilities,
    pub resource: Option<CompletionResource>,
    pub opaque_data: RawBytes,
}

impl NativeCandidate {
    /// Normalize a native candidate into the shared completion model.
    #[must_use]
    pub fn into_completion_item(self, shell: NativeShell) -> CompletionItem {
        CompletionItem {
            id: self.id,
            source: SourceId(shell.source_name().into()),
            label: self.label,
            label_detail: self.label_detail,
            filter_text: self.filter_text,
            sort_text: None,
            kind: self.kind,
            tags: self.tags,
            detail: self.detail,
            documentation: self.documentation,
            group: self.group,
            edit: self.edit,
            insertion: InsertStrategy::NativeMatch {
                shell,
                fingerprint: self.acceptance,
            },
            commit_characters: Vec::new(),
            original_order: self.original_order,
            provider_relevance: 0,
            confidence: self.confidence,
            capabilities: self.capabilities,
            match_result: None,
            resource: self.resource,
            opaque_data: self.opaque_data,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeCandidateBatch {
    pub shell: NativeShell,
    pub candidates: Vec<NativeCandidate>,
    pub is_final: bool,
    pub is_incomplete: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdapterSelector {
    /// Command paths, such as `["git", "checkout"]`. Empty matches all.
    pub command_paths: Vec<Vec<String>>,
    /// Semantic contexts, such as `option` or `path`.
    pub contexts: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContextAdapterDescriptor {
    pub id: SourceId,
    pub display_name: String,
    pub version: Version,
    pub api_version: u16,
    pub capabilities: AdapterCapabilities,
    pub authority: Authority,
    pub selectors: AdapterSelector,
    pub deadlines: DeadlinePolicy,
    pub maximum_concurrency: u16,
    pub maximum_enrichments: u32,
    pub cancellation: bool,
    pub side_effect_free: bool,
}

impl ContextAdapterDescriptor {
    /// Validate compatibility and resource bounds.
    ///
    /// # Errors
    ///
    /// Returns an error when the descriptor is incompatible or unbounded.
    pub fn validate(&self) -> Result<(), ProviderError> {
        validate_api(self.api_version, &self.id.0)?;
        if !self.deadlines.is_valid() {
            return Err(ProviderError::InvalidDescriptor(format!(
                "adapter {} has invalid deadlines",
                self.id.0
            )));
        }
        if self.maximum_concurrency == 0 || self.maximum_enrichments == 0 {
            return Err(ProviderError::InvalidDescriptor(format!(
                "adapter {} must declare non-zero resource bounds",
                self.id.0
            )));
        }
        if self.capabilities.is_empty() {
            return Err(ProviderError::InvalidDescriptor(format!(
                "adapter {} must declare at least one capability",
                self.id.0
            )));
        }
        Ok(())
    }
}

fn validate_api(actual: u16, provider: &str) -> Result<(), ProviderError> {
    if actual == PROVIDER_API_VERSION {
        Ok(())
    } else {
        Err(ProviderError::UnsupportedApiVersion {
            provider: provider.into(),
            expected: PROVIDER_API_VERSION,
            actual,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdapterContext {
    pub request: CompletionRequest,
    /// Tokenization supplied by the active shell. Adapters inspect this
    /// directly instead of applying a second, cross-shell parser.
    pub native_context: NativeCommandContext,
}

#[derive(Debug, Error)]
pub enum ProviderError {
    #[error("provider request was cancelled")]
    Cancelled,
    #[error("provider output channel was closed")]
    OutputClosed,
    #[error("provider {provider} uses API {actual}; expected {expected}")]
    UnsupportedApiVersion {
        provider: String,
        expected: u16,
        actual: u16,
    },
    #[error("invalid provider descriptor: {0}")]
    InvalidDescriptor(String),
    #[error("invalid native query: {0}")]
    InvalidQuery(String),
    #[error("provider failed: {0}")]
    Failed(String),
}

#[derive(Debug, Clone)]
pub struct AdapterSink {
    sender: mpsc::Sender<AdapterEvent>,
}

impl AdapterSink {
    #[must_use]
    pub const fn new(sender: mpsc::Sender<AdapterEvent>) -> Self {
        Self { sender }
    }

    /// Publish an adapter event while remaining responsive to cancellation.
    ///
    /// # Errors
    ///
    /// Returns an error if the operation was cancelled or the receiver closed.
    pub async fn send(
        &self,
        event: AdapterEvent,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        tokio::select! {
            () = cancellation.cancelled() => Err(ProviderError::Cancelled),
            result = self.sender.send(event) => result.map_err(|_| ProviderError::OutputClosed),
        }
    }
}

#[async_trait]
pub trait NativeCompletionProvider: Send + Sync + 'static {
    fn descriptor(&self) -> &NativeProviderDescriptor;

    async fn complete(
        &self,
        context: &NativeCompletionContext,
        cancellation: &CancellationToken,
    ) -> Result<NativeCandidateBatch, ProviderError>;
}

#[async_trait]
pub trait ContextAdapter: Send + Sync + 'static {
    fn descriptor(&self) -> &ContextAdapterDescriptor;

    /// Report whether this adapter can enrich at least one item in the
    /// current native context. The daemon uses this before scheduling async
    /// work, so adapters with narrower item requirements can avoid no-op
    /// jobs and redundant candidate views.
    fn can_enrich(&self, _context: &AdapterContext, _items: &[CompletionItem]) -> bool {
        true
    }

    async fn enrich(
        &self,
        _context: &AdapterContext,
        _items: &[CompletionItem],
        _sink: &AdapterSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn resolve(
        &self,
        _context: &AdapterContext,
        _item: &CompletionItem,
        _sink: &AdapterSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
}

/// Build plain documentation while keeping presentation concerns out of
/// providers.
#[must_use]
pub fn plain_documentation(value: impl Into<String>) -> DocumentationState {
    DocumentationState::Resolved(MarkupContent {
        kind: sense_model::MarkupKind::PlainText,
        value: value.into(),
    })
}
