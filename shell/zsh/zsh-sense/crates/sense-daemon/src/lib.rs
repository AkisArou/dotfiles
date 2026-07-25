//! Persistent, terminal-independent daemon for zsh-sense.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use futures_util::{SinkExt, StreamExt};
use sense_config::Config as ProductConfig;
use sense_model::{
    CompletionItem, CompletionRequest, Generation, ItemId, RequestId, SessionId, SourceId,
};
use sense_protocol::{
    CandidateView, ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolError,
    ProtocolVersion, ServerHello, ServerMessage,
};
use sense_rank::{RankConfig, RankSignals, Ranker};
use thiserror::Error;
use tokio::net::{UnixListener, UnixStream};
use tokio::sync::{Mutex, RwLock, broadcast};
use tokio::task::JoinSet;
use tokio_util::codec::Framed;
use tracing::{debug, warn};

const DEFAULT_EVENT_CAPACITY: usize = 128;
type ClientFramed = Framed<UnixStream, MessagePackCodec<ClientMessage, ServerMessage>>;

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub socket_path: PathBuf,
    pub max_frame_bytes: usize,
    pub event_capacity: usize,
    pub rank_config: RankConfig,
    pub rank_signals: RankSignals,
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
        }
    }

    #[must_use]
    pub fn with_product_config(mut self, config: &ProductConfig) -> Self {
        self.rank_config = RankConfig::from_matching(&config.matching);
        self.rank_signals.source_priorities = [
            (SourceId("zsh".into()), config.sources.zsh.priority),
            (
                SourceId("filesystem".into()),
                config.sources.filesystem.priority,
            ),
            (SourceId("history".into()), config.sources.history.priority),
            (
                SourceId("snippets".into()),
                config.sources.snippets.priority,
            ),
        ]
        .into_iter()
        .collect();
        self
    }
}

#[derive(Debug, Error)]
pub enum DaemonError {
    #[error("daemon I/O failed: {0}")]
    Io(#[from] io::Error),
    #[error("daemon protocol failed: {0}")]
    Protocol(#[from] ProtocolError),
    #[error("zsh-sense is already listening on {0}")]
    AlreadyRunning(PathBuf),
    #[error("refusing to replace non-socket path {0}")]
    UnsafeSocketPath(PathBuf),
    #[error("event capacity must be greater than zero")]
    InvalidEventCapacity,
    #[error("maximum frame size must be between 1 and {} bytes", u32::MAX)]
    InvalidFrameSize,
}

#[derive(Debug)]
struct Session {
    events: broadcast::Sender<ServerMessage>,
    requests: Mutex<HashMap<(RequestId, Generation), ActiveRequest>>,
    ranker: Ranker,
    rank_signals: RankSignals,
}

#[derive(Debug)]
struct ActiveRequest {
    request: CompletionRequest,
    pending_sources: HashSet<SourceId>,
    source_items: HashMap<SourceId, Vec<CompletionItem>>,
    selected: Option<ItemId>,
    revision: u64,
    is_incomplete: bool,
}

impl ActiveRequest {
    fn initial(request: CompletionRequest) -> Self {
        // The scheduler will derive this set from enabled providers. Until
        // provider scheduling lands, the attached Zsh worker is the only
        // candidate producer in the executable vertical slice.
        Self {
            request,
            pending_sources: HashSet::from([SourceId("zsh".into())]),
            source_items: HashMap::new(),
            selected: None,
            revision: 0,
            is_incomplete: false,
        }
    }

    fn candidate_view(&mut self, ranker: &Ranker, signals: &RankSignals) -> CandidateView {
        let mut sources: Vec<_> = self.source_items.iter().collect();
        sources.sort_by(|(left, _), (right, _)| left.0.cmp(&right.0));
        let items: Vec<_> = sources
            .into_iter()
            .flat_map(|(_, items)| items.iter().cloned())
            .collect();
        let query = ranking_query(&self.request, &items);
        let view_results = ranker.rank(&query, items, self.selected.as_ref(), signals);
        self.selected = view_results
            .selected_index
            .and_then(|index| view_results.items.get(index))
            .map(|item| item.id.clone());
        self.revision = self.revision.saturating_add(1);
        let mut sources_pending: Vec<_> = self.pending_sources.iter().cloned().collect();
        sources_pending.sort_by(|left, right| left.0.cmp(&right.0));
        CandidateView {
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
        }
    }
}

#[derive(Debug)]
struct SessionRegistry {
    sessions: RwLock<HashMap<SessionId, Arc<Session>>>,
    event_capacity: usize,
    ranker: Ranker,
    rank_signals: RankSignals,
}

impl SessionRegistry {
    fn new(event_capacity: usize, ranker: Ranker, rank_signals: RankSignals) -> Self {
        Self {
            sessions: RwLock::new(HashMap::new()),
            event_capacity,
            ranker,
            rank_signals,
        }
    }

    async fn negotiate(
        &self,
        hello: &ClientHello,
    ) -> Result<(SessionId, Arc<Session>), HandshakeError> {
        if let Some(session_id) = hello.attach_session {
            let sessions = self.sessions.read().await;
            let session = sessions
                .get(&session_id)
                .cloned()
                .ok_or(HandshakeError::UnknownSession(session_id))?;
            return Ok((session_id, session));
        }

        if matches!(hello.role, PeerRole::CompletionWorker | PeerRole::Adapter) {
            return Err(HandshakeError::SessionRequired);
        }

        let session_id = SessionId::new();
        let (events, _) = broadcast::channel(self.event_capacity);
        let session = Arc::new(Session {
            events,
            requests: Mutex::new(HashMap::new()),
            ranker: self.ranker.clone(),
            rank_signals: self.rank_signals.clone(),
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
    #[error("session {0:?} does not exist")]
    UnknownSession(SessionId),
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
        let registry = Arc::new(SessionRegistry::new(
            config.event_capacity,
            Ranker::new(config.rank_config.clone()),
            config.rank_signals.clone(),
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
    let mut events = session.events.subscribe();
    framed
        .send(ServerMessage::Welcome(ServerHello {
            protocol: ProtocolVersion::CURRENT,
            daemon_version: env!("CARGO_PKG_VERSION").into(),
            session_id,
            max_frame_bytes: u32::try_from(max_frame_bytes).unwrap_or(u32::MAX),
        }))
        .await?;
    debug!(?session_id, role = ?hello.role, "client connected");

    loop {
        tokio::select! {
            incoming = framed.next() => {
                let Some(incoming) = incoming else { break };
                let message = incoming?;
                if handle_client_message(
                    message,
                    session_id,
                    hello.role,
                    &session,
                    &mut framed,
                ).await? {
                    if matches!(hello.role, PeerRole::ZleClient) {
                        registry.remove(session_id).await;
                    }
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
        }
    }
    debug!(?session_id, role = ?hello.role, "client disconnected");
    Ok(())
}

async fn handle_client_message(
    message: ClientMessage,
    session_id: SessionId,
    role: PeerRole,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<bool, DaemonError> {
    match message {
        ClientMessage::Hello(_) => {
            send_error(
                framed,
                "duplicate-hello",
                "hello was already received",
                None,
            )
            .await?;
        }
        ClientMessage::Complete(request) => {
            if matches!(role, PeerRole::ZleClient | PeerRole::Cli) {
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
            if matches!(role, PeerRole::CompletionWorker | PeerRole::Adapter) {
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
        ClientMessage::Cancel {
            session_id: request_session,
            request_id,
            generation,
        } => {
            if matches!(role, PeerRole::ZleClient | PeerRole::Cli) {
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
        ClientMessage::Resolve(resolve) => {
            process_resolve(resolve, session_id, role, session, framed).await?;
        }
        ClientMessage::Ping { nonce } => framed.send(ServerMessage::Pong { nonce }).await?,
        ClientMessage::Goodbye => return Ok(true),
    }
    Ok(false)
}

async fn process_selection(
    selection: sense_protocol::SelectionRequest,
    session_id: SessionId,
    role: PeerRole,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if !matches!(role, PeerRole::ZleClient | PeerRole::Cli) {
        send_error(
            framed,
            "role-not-authorized",
            "this peer role cannot select candidates",
            Some(selection.request_id),
        )
        .await?;
    } else if selection.session_id == session_id {
        publish(session, ServerMessage::SelectionRequested(selection));
    } else {
        send_error(
            framed,
            "wrong-session",
            "selection session does not match this connection",
            Some(selection.request_id),
        )
        .await?;
    }
    Ok(())
}

async fn process_resolve(
    resolve: sense_protocol::ResolveRequest,
    session_id: SessionId,
    role: PeerRole,
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if !matches!(role, PeerRole::ZleClient | PeerRole::Cli) {
        send_error(
            framed,
            "role-not-authorized",
            "this peer role cannot resolve candidates",
            Some(resolve.request_id),
        )
        .await?;
    } else if resolve.session_id == session_id {
        publish(session, ServerMessage::ResolveRequested(resolve));
    } else {
        send_error(
            framed,
            "wrong-session",
            "resolve session does not match this connection",
            Some(resolve.request_id),
        )
        .await?;
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
    session.requests.lock().await.insert(
        (request.request_id, request.generation),
        ActiveRequest::initial(request.clone()),
    );
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
    session: &Session,
    framed: &mut ClientFramed,
) -> Result<(), DaemonError> {
    if batch.session_id == session_id {
        let is_final = batch.is_final;
        let source = batch.source.clone();
        let key = (batch.request_id, batch.generation);
        let mut requests = session.requests.lock().await;
        let Some(request) = requests.get_mut(&key) else {
            drop(requests);
            send_error(
                framed,
                "stale-request",
                "candidate batch does not belong to an active request",
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        };
        if !request.pending_sources.contains(&source) {
            drop(requests);
            send_error(
                framed,
                "unexpected-source",
                "candidate source was not scheduled for this request",
                Some(batch.request_id),
            )
            .await?;
            return Ok(());
        }
        request
            .source_items
            .entry(source.clone())
            .or_default()
            .extend(batch.items);
        request.is_incomplete |= batch.is_incomplete;
        if is_final {
            request.pending_sources.remove(&source);
        }
        let request_finished = request.pending_sources.is_empty();
        let view = request.candidate_view(&session.ranker, &session.rank_signals);
        if request_finished {
            requests.remove(&key);
        }
        drop(requests);
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
    if session
        .requests
        .lock()
        .await
        .remove(&(request_id, generation))
        .is_some()
    {
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
    framed
        .send(ServerMessage::Error {
            code: "handshake-failed".into(),
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
        let mut item =
            CompletionItem::plain("candidate", "zsh", "nvim", TextEdit::new(range, "nvim"));
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
