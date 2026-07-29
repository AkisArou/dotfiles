//! Persistent, terminal-independent daemon for Shell Sense.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

use futures_util::{SinkExt, StreamExt};
use sense_adapters::{
    AdapterRuntime, AdapterSettings, DocumentationAdapterSettings, DocumentationArgument,
    DocumentationResolver, RuntimeConfig as AdapterRuntimeConfig,
};
use sense_config::Config as ProductConfig;
use sense_model::{
    AdapterEvent, CompletionItem, CompletionRequest, Generation, InsertStrategy, ItemCapabilities,
    ItemId, NativeCommandContext, NativeShell, RequestId, SessionId, SourceId,
};
use sense_protocol::{
    CandidateView, ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolError,
    ProtocolVersion, ServerHello, ServerMessage,
};
use sense_provider_api::AdapterContext;
use sense_rank::{RankConfig, RankSignals, Ranker};
use thiserror::Error;
use tokio::net::{UnixListener, UnixStream};
use tokio::sync::{Mutex, RwLock, broadcast};
use tokio::task::JoinSet;
use tokio_util::codec::Framed;
use tokio_util::sync::CancellationToken;
use tracing::{debug, trace, warn};

const DEFAULT_EVENT_CAPACITY: usize = 128;
type ClientFramed = Framed<UnixStream, MessagePackCodec<ClientMessage, ServerMessage>>;

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub socket_path: PathBuf,
    pub max_frame_bytes: usize,
    pub event_capacity: usize,
    pub rank_config: RankConfig,
    pub rank_signals: RankSignals,
    pub adapter_config: AdapterRuntimeConfig,
}

impl ServerConfig {
    #[must_use]
    pub fn new(socket_path: impl Into<PathBuf>) -> Self {
        Self {
            socket_path: socket_path.into(),
            max_frame_bytes: sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            event_capacity: DEFAULT_EVENT_CAPACITY,
            rank_config: RankConfig::default(),
            rank_signals: RankSignals::default(),
            adapter_config: AdapterRuntimeConfig::default(),
        }
    }

    #[must_use]
    pub fn with_product_config(mut self, config: &ProductConfig) -> Self {
        self.rank_config = RankConfig::from_matching(&config.matching);
        self.adapter_config = adapter_runtime_config(config);
        self
    }
}

fn adapter_runtime_config(config: &ProductConfig) -> AdapterRuntimeConfig {
    let adapters = &config.adapters;
    AdapterRuntimeConfig {
        maximum_concurrency: usize::from(adapters.maximum_concurrency),
        documentation_cache_bytes: u64::from(config.cache.memory_mib) * 1024 * 1024,
        documentation_cache_ttl: Duration::from_secs(config.cache.documentation_ttl_seconds),
        documentation: DocumentationAdapterSettings {
            settings: AdapterSettings::new(
                adapters.enabled && adapters.documentation.enabled,
                Duration::from_millis(
                    adapters
                        .documentation
                        .soft_timeout_ms
                        .unwrap_or(adapters.default_soft_timeout_ms),
                ),
                Duration::from_millis(
                    adapters
                        .documentation
                        .hard_timeout_ms
                        .unwrap_or(adapters.default_hard_timeout_ms),
                ),
            ),
            resolvers: adapters
                .documentation
                .resolvers
                .iter()
                .map(|resolver| {
                    let (program, arguments) = resolver
                        .command
                        .split_first()
                        .expect("validated documentation resolver command");
                    DocumentationResolver {
                        name: resolver.name.clone(),
                        kinds: resolver.kinds.clone(),
                        program: program.as_str().into(),
                        arguments: arguments
                            .iter()
                            .map(|argument| match argument.as_str() {
                                "$value" => DocumentationArgument::Value,
                                _ => DocumentationArgument::Literal(argument.as_str().into()),
                            })
                            .collect(),
                    }
                })
                .collect(),
        },
        git: adapter_settings(
            adapters.enabled,
            &adapters.git,
            adapters.default_soft_timeout_ms,
            adapters.default_hard_timeout_ms,
        ),
        man: adapter_settings(
            adapters.enabled,
            &adapters.man,
            adapters.default_soft_timeout_ms,
            adapters.default_hard_timeout_ms,
        ),
        systemd: adapter_settings(
            adapters.enabled,
            &adapters.systemd,
            adapters.default_soft_timeout_ms,
            adapters.default_hard_timeout_ms,
        ),
    }
}

fn adapter_settings(
    globally_enabled: bool,
    adapter: &sense_config::AdapterConfig,
    default_soft_timeout_ms: u64,
    default_hard_timeout_ms: u64,
) -> AdapterSettings {
    AdapterSettings::new(
        globally_enabled && adapter.enabled,
        Duration::from_millis(adapter.soft_timeout_ms.unwrap_or(default_soft_timeout_ms)),
        Duration::from_millis(adapter.hard_timeout_ms.unwrap_or(default_hard_timeout_ms)),
    )
}

#[derive(Debug, Error)]
pub enum DaemonError {
    #[error("daemon I/O failed: {0}")]
    Io(#[from] io::Error),
    #[error("daemon protocol failed: {0}")]
    Protocol(#[from] ProtocolError),
    #[error("shell-sense is already listening on {0}")]
    AlreadyRunning(PathBuf),
    #[error("refusing to replace non-socket path {0}")]
    UnsafeSocketPath(PathBuf),
    #[error("event capacity must be greater than zero")]
    InvalidEventCapacity,
    #[error("maximum frame size must be between 1 and {} bytes", u32::MAX)]
    InvalidFrameSize,
    #[error("adapter runtime configuration is invalid: {0}")]
    InvalidAdapterRuntime(#[from] sense_adapters::RuntimeError),
}

#[derive(Debug)]
struct Session {
    shell: NativeShell,
    owner_process_id: u32,
    native_source: SourceId,
    events: broadcast::Sender<ServerMessage>,
    request: Mutex<Option<ActiveRequest>>,
    pending_selection: Mutex<Option<sense_protocol::SelectionRequest>>,
    ranker: Ranker,
    rank_signals: RankSignals,
    adapters: AdapterRuntime,
    presentation_clients: AtomicUsize,
    lifetime: CancellationToken,
}

#[derive(Debug)]
struct ActiveRequest {
    request: CompletionRequest,
    native_source: SourceId,
    items: Vec<CompletionItem>,
    pending: bool,
    selected: Option<ItemId>,
    native_context: Option<NativeCommandContext>,
    adapter_cancellation: CancellationToken,
    enrichment: EnrichmentState,
    resolving: HashSet<ItemId>,
    revision: u64,
    is_incomplete: bool,
    last_view: Option<CandidateView>,
}

impl ActiveRequest {
    fn initial(request: CompletionRequest, native_source: SourceId) -> Self {
        Self {
            request,
            native_source,
            items: Vec::new(),
            pending: true,
            selected: None,
            native_context: None,
            adapter_cancellation: CancellationToken::new(),
            enrichment: EnrichmentState::Waiting,
            resolving: HashSet::new(),
            revision: 0,
            is_incomplete: false,
            last_view: None,
        }
    }

    fn candidate_view(&mut self, ranker: &Ranker, signals: &RankSignals) -> CandidateView {
        let started = Instant::now();
        let candidate_count = self.items.len();
        let query = ranking_query(&self.request, &self.items);
        let view_results = ranker.rank(&query, self.items.clone(), self.selected.as_ref(), signals);
        trace!(
            request_id = self.request.request_id.0,
            generation = self.request.generation.0,
            candidate_count,
            matched_count = view_results.matched_before_limit,
            result_count = view_results.items.len(),
            elapsed_micros = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX),
            "ranked candidate view"
        );
        self.selected = view_results
            .selected_index
            .and_then(|index| view_results.items.get(index))
            .map(|item| item.id.clone());
        self.revision = self.revision.saturating_add(1);
        let sources_pending: Vec<SourceId> = self
            .pending
            .then(|| self.native_source.clone())
            .into_iter()
            .collect();
        let view = CandidateView {
            session_id: self.request.session_id,
            request_id: self.request.request_id,
            generation: self.request.generation,
            revision: self.revision,
            items: view_results.items,
            selected_index: view_results
                .selected_index
                .and_then(|index| u32::try_from(index).ok()),
            matched_before_limit: u32::try_from(view_results.matched_before_limit)
                .unwrap_or(u32::MAX),
            is_final: sources_pending.is_empty(),
            sources_pending,
            is_incomplete: self.is_incomplete,
            is_settled: !self.pending && self.enrichment == EnrichmentState::Finished,
        };
        self.last_view = Some(view.clone());
        view
    }

    const fn key(&self) -> (RequestId, Generation) {
        (self.request.request_id, self.request.generation)
    }

    fn item(&self, item_id: &ItemId) -> Option<&CompletionItem> {
        self.items.iter().find(|item| &item.id == item_id)
    }

    fn take_enrichment_job(&mut self, adapters: &AdapterRuntime) -> Option<EnrichmentJob> {
        if self.pending || self.enrichment != EnrichmentState::Waiting {
            return None;
        }
        let native_context = self.native_context.clone()?;
        let context = AdapterContext {
            request: self.request.clone(),
            native_context,
        };
        if !adapters.needs_enrichment(&context, &self.items) {
            self.enrichment = EnrichmentState::Finished;
            return None;
        }
        self.enrichment = EnrichmentState::Running;
        Some(EnrichmentJob {
            key: self.key(),
            context,
            items: self.items.clone(),
            cancellation: self.adapter_cancellation.child_token(),
        })
    }

    fn take_resolve_job(
        &mut self,
        item_id: &ItemId,
        adapters: &AdapterRuntime,
    ) -> Option<ResolveJob> {
        let item = self.item(item_id)?;
        if !item
            .capabilities
            .contains(ItemCapabilities::RESOLVE_DOCUMENTATION)
            || self.resolving.contains(item_id)
        {
            return None;
        }
        let native_context = self.native_context.clone()?;
        let context = AdapterContext {
            request: self.request.clone(),
            native_context,
        };
        if !adapters.has_resolver(&context, item) {
            return None;
        }
        let item = item.clone();
        self.resolving.insert(item_id.clone());
        Some(ResolveJob {
            key: self.key(),
            context,
            item,
            cancellation: self.adapter_cancellation.child_token(),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnrichmentState {
    Waiting,
    Running,
    Finished,
}

struct EnrichmentJob {
    key: (RequestId, Generation),
    context: AdapterContext,
    items: Vec<CompletionItem>,
    cancellation: CancellationToken,
}

struct ResolveJob {
    key: (RequestId, Generation),
    context: AdapterContext,
    item: CompletionItem,
    cancellation: CancellationToken,
}

impl Session {
    fn attach_presentation_client(&self) {
        if self.presentation_clients.fetch_add(1, Ordering::AcqRel) == 0 {
            publish(self, ServerMessage::PresentationChanged { external: true });
        }
    }

    fn detach_presentation_client(&self) {
        if self.presentation_clients.fetch_sub(1, Ordering::AcqRel) == 1 {
            publish(self, ServerMessage::PresentationChanged { external: false });
        }
    }

    async fn presentation_snapshot(&self) -> Option<(CompletionRequest, CandidateView)> {
        let current = self.request.lock().await;
        let request = current.as_ref()?;
        Some((request.request.clone(), request.last_view.clone()?))
    }

    fn spawn_enrichment(self: &Arc<Self>, job: EnrichmentJob) {
        let session = Arc::clone(self);
        tokio::spawn(async move {
            let events = session
                .adapters
                .enrich(job.context, job.items, job.cancellation)
                .await;
            session.finish_enrichment(job.key, events).await;
        });
    }

    async fn finish_enrichment(&self, key: (RequestId, Generation), events: Vec<AdapterEvent>) {
        let mut current = self.request.lock().await;
        let Some(request) = current.as_mut().filter(|request| {
            request.key() == key && request.enrichment == EnrichmentState::Running
        }) else {
            return;
        };
        request.enrichment = EnrichmentState::Finished;
        let mut messages: Vec<_> = events
            .into_iter()
            .filter(is_enrichment_event)
            .filter_map(|event| {
                apply_adapter_event(
                    request,
                    event,
                    key.0,
                    key.1,
                    &self.ranker,
                    &self.rank_signals,
                )
            })
            .collect();
        if !messages
            .iter()
            .any(|message| matches!(message, ServerMessage::CandidateView(_)))
        {
            messages.push(ServerMessage::CandidateView(
                request.candidate_view(&self.ranker, &self.rank_signals),
            ));
        }
        drop(current);
        for message in messages {
            publish(self, message);
        }
    }

    fn spawn_resolve(self: &Arc<Self>, job: ResolveJob) {
        let session = Arc::clone(self);
        tokio::spawn(async move {
            let item_id = job.item.id.clone();
            let events = session
                .adapters
                .resolve(job.context, job.item, job.cancellation)
                .await;
            session.finish_resolve(job.key, item_id, events).await;
        });
    }

    async fn finish_resolve(
        &self,
        key: (RequestId, Generation),
        item_id: ItemId,
        events: Vec<AdapterEvent>,
    ) {
        let mut current = self.request.lock().await;
        let Some(request) = current.as_mut().filter(|request| request.key() == key) else {
            return;
        };
        if !request.resolving.remove(&item_id) {
            return;
        }
        let mut resolved = false;
        let mut messages = Vec::new();
        for event in events
            .into_iter()
            .filter(|event| is_resolve_event(event, &item_id))
        {
            resolved |= matches!(&event, AdapterEvent::Documentation { .. });
            if let Some(message) = apply_adapter_event(
                request,
                event,
                key.0,
                key.1,
                &self.ranker,
                &self.rank_signals,
            ) {
                messages.push(message);
            }
        }
        if !resolved && let Some(item) = request.items.iter_mut().find(|item| item.id == item_id) {
            item.capabilities
                .remove(ItemCapabilities::RESOLVE_DOCUMENTATION);
            messages.push(ServerMessage::Documentation {
                request_id: key.0,
                generation: key.1,
                item_id,
                documentation: item.documentation.clone(),
            });
        }
        drop(current);
        for message in messages {
            publish(self, message);
        }
    }
}

const fn is_enrichment_event(event: &AdapterEvent) -> bool {
    matches!(event, AdapterEvent::Enrichments(_))
}

fn is_resolve_event(event: &AdapterEvent, item_id: &ItemId) -> bool {
    matches!(
        event,
        AdapterEvent::Documentation {
            item_id: resolved,
            ..
        } if resolved == item_id
    )
}

#[derive(Debug)]
struct SessionRegistry {
    sessions: RwLock<HashMap<SessionId, Arc<Session>>>,
    event_capacity: usize,
    ranker: Ranker,
    rank_signals: RankSignals,
    adapters: AdapterRuntime,
}

impl SessionRegistry {
    fn new(
        event_capacity: usize,
        ranker: Ranker,
        rank_signals: RankSignals,
        adapters: AdapterRuntime,
    ) -> Self {
        Self {
            sessions: RwLock::new(HashMap::new()),
            event_capacity,
            ranker,
            rank_signals,
            adapters,
        }
    }

    async fn negotiate(
        &self,
        hello: &ClientHello,
    ) -> Result<(SessionId, Arc<Session>), HandshakeError> {
        if hello.attach_session.is_some() && hello.attach_process_id.is_some() {
            return Err(HandshakeError::AmbiguousAttachment);
        }

        if let Some(process_id) = hello.attach_process_id {
            if hello.role != PeerRole::PresentationClient {
                return Err(HandshakeError::ProcessAttachmentRole);
            }
            let sessions = self.sessions.read().await;
            return sessions
                .iter()
                .find(|(_, session)| session.owner_process_id == process_id)
                .map(|(session_id, session)| (*session_id, Arc::clone(session)))
                .ok_or(HandshakeError::UnknownProcess(process_id));
        }

        if let Some(session_id) = hello.attach_session {
            if hello.role == PeerRole::PresentationClient {
                return Err(HandshakeError::ProcessAttachmentRequired);
            }
            let sessions = self.sessions.read().await;
            let session = sessions
                .get(&session_id)
                .cloned()
                .ok_or(HandshakeError::UnknownSession(session_id))?;
            if matches!(hello.role, PeerRole::CompletionWorker) {
                let identity = hello
                    .shell
                    .as_ref()
                    .ok_or(HandshakeError::ShellIdentityRequired)?;
                if identity.shell != session.shell {
                    return Err(HandshakeError::ShellMismatch {
                        expected: session.shell,
                        actual: identity.shell,
                    });
                }
            }
            return Ok((session_id, session));
        }

        if matches!(hello.role, PeerRole::CompletionWorker | PeerRole::Adapter) {
            return Err(HandshakeError::SessionRequired);
        }
        if hello.role == PeerRole::PresentationClient {
            return Err(HandshakeError::ProcessAttachmentRequired);
        }

        let shell = hello
            .shell
            .as_ref()
            .ok_or(HandshakeError::ShellIdentityRequired)?
            .shell;
        let session_id = SessionId::new();
        let (events, _) = broadcast::channel(self.event_capacity);
        let session = Arc::new(Session {
            shell,
            owner_process_id: hello.process_id,
            native_source: SourceId(shell.source_name().into()),
            events,
            request: Mutex::new(None),
            pending_selection: Mutex::new(None),
            ranker: self.ranker.clone(),
            rank_signals: self.rank_signals.clone(),
            adapters: self.adapters.clone(),
            presentation_clients: AtomicUsize::new(0),
            lifetime: CancellationToken::new(),
        });
        self.sessions
            .write()
            .await
            .insert(session_id, Arc::clone(&session));
        Ok((session_id, session))
    }

    async fn remove(&self, session_id: SessionId) {
        self.sessions.write().await.remove(&session_id);
    }
}

#[derive(Debug, Error)]
enum HandshakeError {
    #[error("protocol major {actual} is incompatible with daemon major {expected}")]
    IncompatibleProtocol { expected: u16, actual: u16 },
    #[error("the first message must be a hello")]
    HelloRequired,
    #[error("workers and adapters must attach to an existing session")]
    SessionRequired,
    #[error("a presentation client must attach by owning shell process ID")]
    ProcessAttachmentRequired,
    #[error("only presentation clients may attach by owning shell process ID")]
    ProcessAttachmentRole,
    #[error("a hello cannot attach by both session and process ID")]
    AmbiguousAttachment,
    #[error("a shell identity is required for this peer role")]
    ShellIdentityRequired,
    #[error("worker shell {actual:?} does not match session shell {expected:?}")]
    ShellMismatch {
        expected: NativeShell,
        actual: NativeShell,
    },
    #[error("session {0:?} does not exist")]
    UnknownSession(SessionId),
    #[error("no live shell-sense session belongs to process {0}")]
    UnknownProcess(u32),
}

pub struct Server {
    listener: UnixListener,
    config: ServerConfig,
    registry: Arc<SessionRegistry>,
}

impl Server {
    /// Bind a private Unix socket for the daemon.
    ///
    /// # Errors
    ///
    /// Returns an error for invalid limits, unsafe/still-live socket paths,
    /// permission failures, or listener binding failures.
    pub fn bind(config: ServerConfig) -> Result<Self, DaemonError> {
        if config.event_capacity == 0 {
            return Err(DaemonError::InvalidEventCapacity);
        }
        if config.max_frame_bytes == 0 || u32::try_from(config.max_frame_bytes).is_err() {
            return Err(DaemonError::InvalidFrameSize);
        }
        prepare_socket_path(&config.socket_path)?;
        let listener = UnixListener::bind(&config.socket_path)?;
        fs::set_permissions(&config.socket_path, fs::Permissions::from_mode(0o600))?;
        let adapters = AdapterRuntime::new(config.adapter_config.clone())?;
        let registry = Arc::new(SessionRegistry::new(
            config.event_capacity,
            Ranker::new(config.rank_config.clone()),
            config.rank_signals.clone(),
            adapters,
        ));
        Ok(Self {
            listener,
            config,
            registry,
        })
    }

    #[must_use]
    pub fn socket_path(&self) -> &Path {
        &self.config.socket_path
    }

    /// Serve clients until the process is terminated.
    ///
    /// # Errors
    ///
    /// Returns an error if accepting a client fails.
    pub async fn run(self) -> Result<(), DaemonError> {
        self.run_until(std::future::pending::<()>()).await
    }

    /// Serve clients until `shutdown` resolves.
    ///
    /// # Errors
    ///
    /// Returns an error if accepting a client fails.
    pub async fn run_until<F>(self, shutdown: F) -> Result<(), DaemonError>
    where
        F: Future<Output = ()>,
    {
        let mut tasks = JoinSet::new();
        tokio::pin!(shutdown);

        loop {
            tokio::select! {
                () = &mut shutdown => break,
                result = self.listener.accept() => {
                    let (stream, _) = result?;
                    let registry = Arc::clone(&self.registry);
                    let max_frame_bytes = self.config.max_frame_bytes;
                    tasks.spawn(async move {
                        if let Err(error) = handle_connection(stream, registry, max_frame_bytes).await {
                            warn!(%error, "client connection ended with an error");
                        }
                    });
                }
                Some(result) = tasks.join_next(), if !tasks.is_empty() => {
                    if let Err(error) = result {
                        warn!(%error, "client task panicked or was cancelled");
                    }
                }
            }
        }

        tasks.abort_all();
        while tasks.join_next().await.is_some() {}
        Ok(())
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        if let Err(error) = fs::remove_file(&self.config.socket_path)
            && error.kind() != io::ErrorKind::NotFound
        {
            warn!(
                path = %self.config.socket_path.display(),
                %error,
                "could not remove daemon socket"
            );
        }
    }
}

fn prepare_socket_path(path: &Path) -> Result<(), DaemonError> {
    if let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        match fs::metadata(parent) {
            Ok(metadata) if metadata.is_dir() => {}
            Ok(_) => return Err(DaemonError::UnsafeSocketPath(parent.to_path_buf())),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                fs::create_dir_all(parent)?;
                fs::set_permissions(parent, fs::Permissions::from_mode(0o700))?;
            }
            Err(error) => return Err(error.into()),
        }
    }

    let Ok(metadata) = fs::symlink_metadata(path) else {
        return Ok(());
    };
    if !metadata.file_type().is_socket() {
        return Err(DaemonError::UnsafeSocketPath(path.to_path_buf()));
    }

    match std::os::unix::net::UnixStream::connect(path) {
        Ok(_) => Err(DaemonError::AlreadyRunning(path.to_path_buf())),
        Err(error)
            if matches!(
                error.kind(),
                io::ErrorKind::ConnectionRefused | io::ErrorKind::NotFound
            ) =>
        {
            fs::remove_file(path)?;
            Ok(())
        }
        Err(error) => Err(DaemonError::Io(error)),
    }
}

async fn handle_connection(
    stream: UnixStream,
    registry: Arc<SessionRegistry>,
    max_frame_bytes: usize,
) -> Result<(), DaemonError> {
    let codec = MessagePackCodec::<ClientMessage, ServerMessage>::new(max_frame_bytes);
    let mut framed = Framed::new(stream, codec);
    let hello = match framed.next().await {
        Some(Ok(ClientMessage::Hello(hello))) => hello,
        Some(Ok(_)) => {
            send_handshake_error(&mut framed, HandshakeError::HelloRequired).await?;
            return Ok(());
        }
        Some(Err(error)) => return Err(error.into()),
        None => return Ok(()),
    };

    if !hello.protocol.is_compatible_with(ProtocolVersion::CURRENT) {
        send_handshake_error(
            &mut framed,
            HandshakeError::IncompatibleProtocol {
                expected: ProtocolVersion::CURRENT.major,
                actual: hello.protocol.major,
            },
        )
        .await?;
        return Ok(());
    }

    let (session_id, session) = match registry.negotiate(&hello).await {
        Ok(negotiated) => negotiated,
        Err(error) => {
            send_handshake_error(&mut framed, error).await?;
            return Ok(());
        }
    };

    let role = hello.role;
    let result = run_connection(&mut framed, session_id, role, &session, max_frame_bytes).await;
    if role == PeerRole::ShellClient {
        session.lifetime.cancel();
        registry.remove(session_id).await;
    }
    debug!(?session_id, ?role, "client disconnected");
    result
}

async fn run_connection(
    framed: &mut ClientFramed,
    session_id: SessionId,
    role: PeerRole,
    session: &Arc<Session>,
    max_frame_bytes: usize,
) -> Result<(), DaemonError> {
    let mut events = session.events.subscribe();
    framed
        .send(ServerMessage::Welcome(ServerHello {
            protocol: ProtocolVersion::CURRENT,
            daemon_version: env!("CARGO_PKG_VERSION").into(),
            session_id,
            max_frame_bytes: u32::try_from(max_frame_bytes).unwrap_or(u32::MAX),
        }))
        .await?;
    let _presentation_registration = (role == PeerRole::PresentationClient)
        .then(|| PresentationRegistration::new(Arc::clone(session)));
    if role == PeerRole::PresentationClient
        && let Some((request, view)) = session.presentation_snapshot().await
    {
        framed
            .send(ServerMessage::CompletionRequested(request))
            .await?;
        framed.send(ServerMessage::CandidateView(view)).await?;
    }
    debug!(?session_id, ?role, "client connected");

    loop {
        tokio::select! {
            incoming = framed.next() => {
                let Some(incoming) = incoming else { break };
                let message = incoming?;
                if handle_client_message(
                    message,
                    session_id,
                    role,
                    session,
                    framed,
                ).await? {
                    break;
                }
            }
            event = events.recv() => {
                match event {
                    Ok(message) => framed.send(message).await?,
                    Err(broadcast::error::RecvError::Lagged(skipped)) => {
                        framed.send(ServerMessage::Error {
                            code: "client-lagged".into(),
                            message: format!("client skipped {skipped} session events"),
                            request_id: None,
                        }).await?;
                    }
                    Err(broadcast::error::RecvError::Closed) => break,
                }
            }
            () = session.lifetime.cancelled(), if role != PeerRole::ShellClient => break,
        }
    }
    Ok(())
}

struct PresentationRegistration {
    session: Arc<Session>,
}

impl PresentationRegistration {
    fn new(session: Arc<Session>) -> Self {
        session.attach_presentation_client();
        Self { session }
    }
}

impl Drop for PresentationRegistration {
    fn drop(&mut self) {
        self.session.detach_presentation_client();
    }
}

async fn handle_client_message(
    message: ClientMessage,
    session_id: SessionId,
    role: PeerRole,
    session: &Arc<Session>,
    framed: &mut ClientFramed,
) -> Result<bool, DaemonError> {
    match message {
        ClientMessage::Hello(_) => send_duplicate_hello(framed).await?,
        ClientMessage::Complete(request) => {
            if role == PeerRole::ShellClient {
                process_completion(request, session_id, session, framed).await?;
            } else {
                send_error(
                    framed,
                    "role-not-authorized",
                    "this peer role cannot start completion requests",
                    Some(request.request_id),
                )
                .await?;
            }
        }
        ClientMessage::PublishCandidates(batch) => {
            if matches!(role, PeerRole::CompletionWorker) {
                process_candidate_batch(batch, session_id, session, framed).await?;
            } else {
                send_error(
                    framed,
                    "role-not-authorized",
                    "this peer role cannot publish candidates",
                    Some(batch.request_id),
                )
                .await?;
            }
        }
        ClientMessage::PublishNativeContext(publication) => {
            if matches!(role, PeerRole::CompletionWorker) {
                process_native_context(publication, session_id, session, framed).await?;
            } else {
                send_error(
                    framed,
                    "role-not-authorized",
                    "this peer role cannot publish native command context",
                    Some(publication.request_id),
                )
                .await?;
            }
        }
        ClientMessage::PublishAdapterEvent(publication) => {
            if matches!(role, PeerRole::Adapter) {
                process_adapter_event(publication, session_id, session, framed).await?;
            } else {
                send_error(
                    framed,
                    "role-not-authorized",
                    "this peer role cannot publish adapter events",
                    Some(publication.request_id),
                )
                .await?;
            }
        }
        ClientMessage::Cancel {
            session_id: request_session,
            request_id,
            generation,
        } => {
            if role == PeerRole::ShellClient {
                process_cancellation(
                    request_session,
                    request_id,
                    generation,
                    session_id,
                    session,
                    framed,
                )
                .await?;
            } else {
                send_error(
                    framed,
                    "role-not-authorized",
                    "this peer role cannot cancel requests",
                    Some(request_id),
                )
                .await?;
            }
        }
        ClientMessage::Select(selection) => {
            process_selection(selection, session_id, role, session, framed).await?;
        }
        ClientMessage::ReportSelection(result) => {
            process_selection_result(result, session_id, role, session, framed).await?;
        }
        ClientMessage::Resolve(resolve) => {
            process_resolve(resolve, session_id, role, session, framed).await?;
        }
        ClientMessage::Ping { nonce } => framed.send(ServerMessage::Pong { nonce }).await?,
        ClientMessage::Goodbye => return Ok(true),
    }
    Ok(false)
}

async fn send_duplicate_hello(framed: &mut ClientFramed) -> Result<(), ProtocolError> {
    send_error(
        framed,
        "duplicate-hello",
        "hello was already received",
        None,
    )
    .await
}

async fn process_native_context(
    publication: sense_protocol::NativeContextPublication,
    session_id: SessionId,
    session: &Arc<Session>,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if publication.session_id != session_id {
        return send_error(
            framed,
            "wrong-session",
            "native context session does not match this connection",
            Some(publication.request_id),
        )
        .await
        .map_err(Into::into);
    }
    if !publication.context.is_valid() {
        return send_error(
            framed,
            "invalid-native-context",
            "current word is outside the native word vector",
            Some(publication.request_id),
        )
        .await
        .map_err(Into::into);
    }
    let key = (publication.request_id, publication.generation);
    let mut current = session.request.lock().await;
    let Some(request) = current.as_mut().filter(|request| request.key() == key) else {
        drop(current);
        return send_error(
            framed,
            "stale-request",
            "native context does not belong to the active request",
            Some(publication.request_id),
        )
        .await
        .map_err(Into::into);
    };
    request.native_context = Some(publication.context.clone());
    let enrichment = request.take_enrichment_job(&session.adapters);
    let settled_view = (!request.pending && request.enrichment == EnrichmentState::Finished)
        .then(|| request.candidate_view(&session.ranker, &session.rank_signals));
    drop(current);
    publish(session, ServerMessage::NativeContextPublished(publication));
    if let Some(view) = settled_view {
        publish(session, ServerMessage::CandidateView(view));
    }
    if let Some(job) = enrichment {
        session.spawn_enrichment(job);
    }
    Ok(())
}

async fn process_adapter_event(
    publication: sense_protocol::AdapterEventPublication,
    session_id: SessionId,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if publication.session_id != session_id {
        return send_error(
            framed,
            "wrong-session",
            "adapter event session does not match this connection",
            Some(publication.request_id),
        )
        .await
        .map_err(Into::into);
    }
    let key = (publication.request_id, publication.generation);
    let mut current = session.request.lock().await;
    let Some(request) = current.as_mut().filter(|request| request.key() == key) else {
        drop(current);
        return send_error(
            framed,
            "stale-request",
            "adapter event does not belong to the active request",
            Some(publication.request_id),
        )
        .await
        .map_err(Into::into);
    };
    let event = apply_adapter_event(
        request,
        publication.event,
        publication.request_id,
        publication.generation,
        &session.ranker,
        &session.rank_signals,
    );
    drop(current);
    if let Some(event) = event {
        publish(session, event);
    }
    Ok(())
}

fn apply_adapter_event(
    request: &mut ActiveRequest,
    event: AdapterEvent,
    request_id: RequestId,
    generation: Generation,
    ranker: &Ranker,
    rank_signals: &RankSignals,
) -> Option<ServerMessage> {
    match event {
        AdapterEvent::Enrichments(enrichments) => apply_enrichments(request, enrichments)
            .then(|| ServerMessage::CandidateView(request.candidate_view(ranker, rank_signals))),
        AdapterEvent::Documentation {
            item_id,
            documentation,
        } => {
            request.resolving.remove(&item_id);
            let item = request.items.iter_mut().find(|item| item.id == item_id)?;
            item.documentation = documentation.clone();
            item.capabilities
                .remove(ItemCapabilities::RESOLVE_DOCUMENTATION);
            Some(ServerMessage::Documentation {
                request_id,
                generation,
                item_id,
                documentation,
            })
        }
    }
}

fn apply_enrichments(
    request: &mut ActiveRequest,
    enrichments: Vec<sense_model::Enrichment>,
) -> bool {
    let mut changed = false;
    for enrichment in enrichments {
        let Some(item) = request
            .items
            .iter_mut()
            .find(|item| item.id == enrichment.item_id)
        else {
            continue;
        };
        if let Some(kind) = enrichment.kind {
            item.kind = kind;
        }
        item.tags.insert(enrichment.add_tags);
        item.capabilities.insert(enrichment.add_capabilities);
        if let Some(detail) = enrichment.detail {
            item.detail = Some(detail);
        }
        if let Some(documentation) = enrichment.documentation {
            item.documentation = documentation;
        }
        changed = true;
    }
    changed
}

async fn process_selection(
    selection: sense_protocol::SelectionRequest,
    session_id: SessionId,
    role: PeerRole,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if !matches!(role, PeerRole::ShellClient | PeerRole::PresentationClient) {
        send_error(
            framed,
            "role-not-authorized",
            "this peer role cannot select candidates",
            Some(selection.request_id),
        )
        .await?;
    } else if selection.session_id != session_id {
        send_error(
            framed,
            "wrong-session",
            "selection session does not match this connection",
            Some(selection.request_id),
        )
        .await?;
    } else {
        let request = session.request.lock().await;
        let expected_key = (selection.request_id, selection.generation);
        let Some(current) = request
            .as_ref()
            .filter(|current| current.key() == expected_key)
        else {
            drop(request);
            send_error(
                framed,
                "stale-request",
                "selection does not belong to the current completion generation",
                Some(selection.request_id),
            )
            .await?;
            return Ok(());
        };
        if current.item(&selection.item_id).is_none() {
            drop(request);
            send_error(
                framed,
                "unknown-item",
                "selection item does not belong to the current completion generation",
                Some(selection.request_id),
            )
            .await?;
            return Ok(());
        }
        drop(request);
        *session.pending_selection.lock().await = Some(selection.clone());
        publish(session, ServerMessage::SelectionRequested(selection));
    }
    Ok(())
}

async fn process_selection_result(
    result: sense_protocol::SelectionResult,
    session_id: SessionId,
    role: PeerRole,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    let selection = &result.selection;
    let may_report =
        role == PeerRole::ShellClient || (role == PeerRole::CompletionWorker && !result.applied);
    if !may_report {
        return send_error(
            framed,
            "role-not-authorized",
            "only the owning shell may report application; the native worker may only reject before application",
            Some(selection.request_id),
        )
        .await
        .map_err(Into::into);
    }
    if selection.session_id != session_id {
        return send_error(
            framed,
            "wrong-session",
            "selection result session does not match this connection",
            Some(selection.request_id),
        )
        .await
        .map_err(Into::into);
    }
    let mut pending = session.pending_selection.lock().await;
    if pending.as_ref() != Some(selection) {
        drop(pending);
        return send_error(
            framed,
            "unknown-selection",
            "selection result does not match the pending native selection",
            Some(selection.request_id),
        )
        .await
        .map_err(Into::into);
    }
    *pending = None;
    drop(pending);
    publish(session, ServerMessage::SelectionFinished(result));
    Ok(())
}

async fn process_resolve(
    resolve: sense_protocol::ResolveRequest,
    session_id: SessionId,
    role: PeerRole,
    session: &Arc<Session>,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if !matches!(role, PeerRole::ShellClient | PeerRole::PresentationClient) {
        send_error(
            framed,
            "role-not-authorized",
            "this peer role cannot resolve candidates",
            Some(resolve.request_id),
        )
        .await?;
    } else if resolve.session_id != session_id {
        send_error(
            framed,
            "wrong-session",
            "resolve session does not match this connection",
            Some(resolve.request_id),
        )
        .await?;
    } else {
        let mut request = session.request.lock().await;
        let expected_key = (resolve.request_id, resolve.generation);
        let Some(current) = request
            .as_mut()
            .filter(|current| current.key() == expected_key)
        else {
            drop(request);
            send_error(
                framed,
                "stale-request",
                "documentation request does not belong to the current completion generation",
                Some(resolve.request_id),
            )
            .await?;
            return Ok(());
        };
        let Some(documentation) = current
            .item(&resolve.item_id)
            .map(|item| item.documentation.clone())
        else {
            drop(request);
            send_error(
                framed,
                "unknown-item",
                "documentation item does not belong to the current completion generation",
                Some(resolve.request_id),
            )
            .await?;
            return Ok(());
        };
        let adapter_capability = current.item(&resolve.item_id).is_some_and(|item| {
            item.capabilities
                .contains(ItemCapabilities::RESOLVE_DOCUMENTATION)
        });
        let adapter_job = current.take_resolve_job(&resolve.item_id, &session.adapters);
        let needs_resolution = adapter_capability
            || matches!(documentation, sense_model::DocumentationState::Unresolved);
        drop(request);
        if needs_resolution {
            publish(session, ServerMessage::ResolveRequested(resolve));
            if let Some(job) = adapter_job {
                session.spawn_resolve(job);
            }
        } else {
            publish(
                session,
                ServerMessage::Documentation {
                    request_id: resolve.request_id,
                    generation: resolve.generation,
                    item_id: resolve.item_id,
                    documentation,
                },
            );
        }
    }
    Ok(())
}

async fn process_completion(
    request: sense_model::CompletionRequest,
    session_id: SessionId,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if request.session_id != session_id {
        return send_error(
            framed,
            "wrong-session",
            "request session does not match this connection",
            Some(request.request_id),
        )
        .await
        .map_err(Into::into);
    }
    if !request.cursor_is_valid() {
        return send_error(
            framed,
            "invalid-cursor",
            "cursor is outside the command buffer",
            Some(request.request_id),
        )
        .await
        .map_err(Into::into);
    }
    let mut current = session.request.lock().await;
    if current
        .as_ref()
        .is_some_and(|active| active.request.generation >= request.generation)
    {
        drop(current);
        return send_error(
            framed,
            "stale-generation",
            "completion generation must increase monotonically",
            Some(request.request_id),
        )
        .await
        .map_err(Into::into);
    }
    let replaced = current.replace(ActiveRequest::initial(
        request.clone(),
        session.native_source.clone(),
    ));
    drop(current);
    if let Some(replaced) = replaced {
        replaced.adapter_cancellation.cancel();
        let (request_id, generation) = replaced.key();
        publish(
            session,
            ServerMessage::RequestCancelled {
                request_id,
                generation,
            },
        );
        publish(
            session,
            ServerMessage::RequestFinished {
                request_id,
                generation,
                cancelled: true,
            },
        );
    }
    publish(
        session,
        ServerMessage::RequestStarted {
            request_id: request.request_id,
            generation: request.generation,
        },
    );
    publish(session, ServerMessage::CompletionRequested(request));
    Ok(())
}

async fn process_candidate_batch(
    batch: sense_protocol::CandidateBatch,
    session_id: SessionId,
    session: &Arc<Session>,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if batch.session_id == session_id {
        let is_final = batch.is_final;
        let source = batch.source.clone();
        let key = (batch.request_id, batch.generation);
        let mut current = session.request.lock().await;
        let Some(request) = current.as_mut().filter(|request| request.key() == key) else {
            drop(current);
            send_error(
                framed,
                "stale-request",
                "candidate batch does not belong to an active request",
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        };
        if !request.pending {
            drop(current);
            send_error(
                framed,
                "request-finished",
                "candidate batch belongs to an already completed request",
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        }
        if source != request.native_source {
            drop(current);
            send_error(
                framed,
                "non-native-source",
                "candidate source does not match the session's native shell",
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        }
        if let Some(error) = validate_native_items(&batch.items, session.shell, &source) {
            drop(current);
            send_error(
                framed,
                "invalid-native-candidate",
                error,
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        }
        request.items.extend(batch.items);
        request.is_incomplete |= batch.is_incomplete;
        if is_final {
            request.pending = false;
        }
        let request_finished = !request.pending;
        let enrichment = request.take_enrichment_job(&session.adapters);
        let view = request.candidate_view(&session.ranker, &session.rank_signals);
        drop(current);
        publish(session, ServerMessage::CandidateView(view));
        if request_finished {
            publish(
                session,
                ServerMessage::RequestFinished {
                    request_id: key.0,
                    generation: key.1,
                    cancelled: false,
                },
            );
        }
        if let Some(job) = enrichment {
            session.spawn_enrichment(job);
        }
        return Ok(());
    }
    send_error(
        framed,
        "wrong-session",
        "candidate batch session does not match this connection",
        Some(batch.request_id),
    )
    .await?;
    Ok(())
}

fn validate_native_items(
    items: &[CompletionItem],
    shell: NativeShell,
    source: &SourceId,
) -> Option<&'static str> {
    for item in items {
        if &item.source != source {
            return Some("candidate item source differs from its native batch source");
        }
        let InsertStrategy::NativeMatch {
            shell: item_shell, ..
        } = item.insertion;
        if item_shell != shell {
            return Some("candidate acceptance belongs to a different native shell");
        }
    }
    None
}

fn ranking_query(request: &CompletionRequest, items: &[CompletionItem]) -> String {
    let Some(range) = items.iter().map(|item| item.edit.range).find(|range| {
        range.is_valid_for(request.buffer.as_slice())
            && range.start <= request.cursor
            && request.cursor <= range.end
    }) else {
        return String::new();
    };

    // Only text to the left of the cursor is a filter query. The remainder of
    // a replacement range is an edit suffix, not text the candidate must
    // match. This also keeps completion correct when editing in the middle of
    // an existing word.
    let Ok(mut query) = std::str::from_utf8(
        &request.buffer.as_slice()[range.start.as_usize()..request.cursor.as_usize()],
    ) else {
        return String::new();
    };

    // Zsh path matches use the whole shell word as their replacement range,
    // while the inserted/filter text names only the active path component.
    // Rank that component (`nv` in `dotfiles/nv`), including the empty
    // component after a slash, and leave Zsh responsible for path insertion.
    let same_range = items.iter().filter(|item| item.edit.range == range);
    if same_range
        .clone()
        .next()
        .is_some_and(|_| same_range.clone().all(|item| is_path_item(item.kind)))
    {
        query = query.rsplit_once('/').map_or(query, |(_, tail)| tail);
    }
    query.to_owned()
}

const fn is_path_item(kind: sense_model::CompletionKind) -> bool {
    matches!(
        kind,
        sense_model::CompletionKind::File
            | sense_model::CompletionKind::Directory
            | sense_model::CompletionKind::Symlink
    )
}

async fn process_cancellation(
    request_session: SessionId,
    request_id: RequestId,
    generation: Generation,
    session_id: SessionId,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if request_session != session_id {
        send_error(
            framed,
            "wrong-session",
            "cancellation session does not match this connection",
            Some(request_id),
        )
        .await?;
        return Ok(());
    }
    let mut current = session.request.lock().await;
    let removed = if current
        .as_ref()
        .is_some_and(|request| request.key() == (request_id, generation))
    {
        current.take()
    } else {
        None
    };
    drop(current);
    if let Some(removed) = removed {
        removed.adapter_cancellation.cancel();
        publish(
            session,
            ServerMessage::RequestCancelled {
                request_id,
                generation,
            },
        );
        publish(
            session,
            ServerMessage::RequestFinished {
                request_id,
                generation,
                cancelled: true,
            },
        );
    }
    Ok(())
}

fn publish(session: &Session, message: ServerMessage) {
    // A session always has at least the current connection's receiver. A send
    // failure therefore only means every client disconnected concurrently.
    let _ = session.events.send(message);
}

async fn send_handshake_error(
    framed: &mut ClientFramed,
    error: HandshakeError,
) -> Result<(), ProtocolError> {
    let code = if matches!(error, HandshakeError::UnknownProcess(_)) {
        "shell-session-unavailable"
    } else {
        "handshake-failed"
    };
    framed
        .send(ServerMessage::Error {
            code: code.into(),
            message: error.to_string(),
            request_id: None,
        })
        .await
}

async fn send_error(
    framed: &mut ClientFramed,
    code: &str,
    message: &str,
    request_id: Option<RequestId>,
) -> Result<(), ProtocolError> {
    framed
        .send(ServerMessage::Error {
            code: code.into(),
            message: message.into(),
            request_id,
        })
        .await
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use sense_model::{
        ByteOffset, CompletionKind, ContextEpoch, RawBytes, TerminalDimensions, TextEdit,
        TextRange, TriggerKind,
    };

    use super::*;

    fn completion_request(buffer: &str, cursor: u32) -> CompletionRequest {
        CompletionRequest {
            session_id: SessionId::new(),
            request_id: RequestId(1),
            generation: Generation(1),
            context_epoch: ContextEpoch::default(),
            buffer: RawBytes::from(buffer),
            cursor: ByteOffset(cursor),
            cwd: RawBytes::from("/tmp"),
            keymap: "emacs".into(),
            terminal: TerminalDimensions::default(),
            trigger: TriggerKind::Automatic,
            environment: BTreeMap::new(),
        }
    }

    fn item(kind: CompletionKind, range: TextRange) -> CompletionItem {
        let mut item = CompletionItem::native(
            "candidate",
            NativeShell::Zsh,
            "nvim",
            TextEdit::new(range, "nvim"),
            "candidate",
        );
        item.kind = kind;
        item
    }

    #[test]
    fn ranking_query_uses_only_text_left_of_the_cursor() {
        let request = completion_request("command restaXYZ", 13);
        let items = [item(CompletionKind::Subcommand, TextRange::new(8, 16))];

        assert_eq!(ranking_query(&request, &items), "resta");
    }

    #[test]
    fn ranking_query_uses_the_active_path_component() {
        let request = completion_request("cd dotfiles/nv", 14);
        let items = [item(CompletionKind::File, TextRange::new(3, 14))];

        assert_eq!(ranking_query(&request, &items), "nv");

        let request = completion_request("cd dotfiles/", 12);
        let items = [item(CompletionKind::Directory, TextRange::new(3, 12))];
        assert_eq!(ranking_query(&request, &items), "");
    }
}
