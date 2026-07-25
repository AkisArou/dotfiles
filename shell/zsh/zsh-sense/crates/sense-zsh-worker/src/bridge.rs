//! Persistent bridge between one interactive Zsh process and the daemon.

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs::{self, OpenOptions};
use std::io;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::path::PathBuf;
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use sense_model::{
    ByteOffset, CompletionItem, CompletionKind, CompletionRequest, ContextEpoch, Generation,
    ItemId, RawBytes, RequestId, SessionId, TerminalDimensions, TextRange, TriggerKind,
};
use sense_protocol::{
    CandidateBatch, CandidateView, ClientHello, ClientMessage, MessagePackCodec, PeerRole,
    ProtocolError, ProtocolVersion, ServerHello, ServerMessage, ZshIdentity,
};
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio::net::UnixStream;
use tokio_util::codec::{Framed, FramedRead, FramedWrite};

use crate::{
    CaptureBackend, CaptureError, CaptureLimits, CaptureStore, CapturedGroup, CapturedMatch,
    ShellWireCodec, ShellWireError, ShellWireLimits, ShellWireMessage, ZshInsertionMetadata,
    ZshMatchFlags,
};

type DaemonConnection = Framed<UnixStream, MessagePackCodec<ServerMessage, ClientMessage>>;
type RequestKey = (RequestId, Generation);

const MAX_CANCELLED_CAPTURE_TOMBSTONES: usize = 256;
const VIEW_CHUNK_ITEM_FIELDS: usize = 7;
// 3 envelope fields + (16 * 7) item fields = 115, below the default and
// shell-side 128-field wire limit.
const VIEW_CHUNK_ITEMS: usize = 16;
const SHELL_OUTPUT_BACKPRESSURE_BYTES: usize = 256 * 1024;

#[derive(Debug, Clone)]
pub struct BridgeConfig {
    pub socket_path: PathBuf,
    pub daemon_frame_bytes: usize,
    pub shell_wire_limits: ShellWireLimits,
    pub capture_limits: CaptureLimits,
    pub debounce: Duration,
    pub viewport_rows: usize,
    pub startup_messages: Vec<ShellWireMessage>,
    pub client_version: String,
    pub zsh: Option<ZshIdentity>,
}

impl BridgeConfig {
    #[must_use]
    pub fn new(socket_path: impl Into<PathBuf>) -> Self {
        Self {
            socket_path: socket_path.into(),
            daemon_frame_bytes: sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            shell_wire_limits: ShellWireLimits::default(),
            capture_limits: CaptureLimits::default(),
            debounce: Duration::from_millis(15),
            viewport_rows: 20,
            startup_messages: Vec::new(),
            client_version: env!("CARGO_PKG_VERSION").into(),
            zsh: None,
        }
    }
}

#[derive(Debug, Error)]
pub enum BridgeError {
    #[error("worker bridge I/O failed: {0}")]
    Io(#[from] io::Error),
    #[error("worker bridge daemon protocol failed: {0}")]
    Protocol(#[from] ProtocolError),
    #[error("worker bridge shell protocol failed: {0}")]
    ShellWire(#[from] ShellWireError),
    #[error("worker bridge capture failed: {0}")]
    Capture(#[from] CaptureError),
    #[error("daemon frame size must be greater than zero")]
    InvalidDaemonFrameSize,
    #[error("{path} is not a FIFO")]
    InvalidFifo { path: PathBuf },
    #[error("FIFO {path} is accessible by group or other users")]
    InsecureFifo { path: PathBuf },
    #[error("FIFO open task failed: {0}")]
    FifoTask(#[from] tokio::task::JoinError),
    #[error("worker bridge could not size a candidate batch: {0}")]
    CandidateEncoding(#[from] rmp_serde::encode::Error),
    #[error("candidate {item_id} needs a {actual}-byte daemon frame; limit is {limit} bytes")]
    CandidateFrameTooLarge {
        item_id: String,
        actual: usize,
        limit: usize,
    },
    #[error("daemon closed the {role} connection")]
    DaemonClosed { role: &'static str },
    #[error("daemon rejected the {role} handshake: {code}: {message}")]
    HandshakeRejected {
        role: &'static str,
        code: String,
        message: String,
    },
    #[error("daemon sent {message} instead of a welcome to the {role}")]
    UnexpectedHandshake { role: &'static str, message: String },
    #[error("invalid shell command {command}: {message}")]
    InvalidShellCommand { command: String, message: String },
    #[error(
        "candidate capture was not started for request {request_id:?} generation {generation:?}"
    )]
    CaptureNotStarted {
        request_id: RequestId,
        generation: Generation,
    },
    #[error(
        "candidate capture already exists for request {request_id:?} generation {generation:?}"
    )]
    CaptureAlreadyStarted {
        request_id: RequestId,
        generation: Generation,
    },
    #[error(
        "candidate capture references an unknown request {request_id:?} generation {generation:?}"
    )]
    UnknownRequest {
        request_id: RequestId,
        generation: Generation,
    },
}

#[derive(Debug)]
struct PendingCapture {
    backend: CaptureBackend,
    matches: Vec<CapturedMatch>,
    retained_bytes: usize,
    dropped: usize,
}

#[derive(Debug)]
enum ShellInput {
    Complete(CompletionRequest),
    Cancel(RequestId, Generation),
    Select(RequestId, Generation, ItemId),
    Navigate(RequestId, Generation, Navigation),
    CaptureBegin(RequestId, Generation, CaptureBackend),
    Candidate(RequestId, Generation, Box<CapturedMatch>),
    CandidateChunk(RequestId, Generation, Vec<CapturedMatch>),
    CaptureEnd(RequestId, Generation),
    Ping(u64),
    Goodbye,
}

#[derive(Debug, Clone, Copy)]
enum Navigation {
    Next,
    Previous,
    PageDown,
    PageUp,
}

#[derive(Debug)]
struct CachedView {
    view: CandidateView,
    selected: usize,
}

#[derive(Debug)]
struct WindowedView {
    view: CandidateView,
    total: usize,
    start: usize,
    selected_absolute: usize,
}

#[derive(Debug)]
struct BridgeState {
    session_id: SessionId,
    daemon_frame_bytes: usize,
    capture_limits: CaptureLimits,
    requests: HashMap<(RequestId, Generation), CompletionRequest>,
    pending: HashMap<(RequestId, Generation), PendingCapture>,
    capture_store: CaptureStore,
    pending_completion: Option<CompletionRequest>,
    cancelled_captures: HashSet<RequestKey>,
    cancelled_capture_order: VecDeque<RequestKey>,
    highest_generation: u64,
    viewport_rows: usize,
    current_view: Option<CachedView>,
}

impl BridgeState {
    fn new(
        session_id: SessionId,
        daemon_frame_bytes: usize,
        capture_limits: CaptureLimits,
        viewport_rows: usize,
    ) -> Result<Self, CaptureError> {
        Ok(Self {
            session_id,
            daemon_frame_bytes,
            capture_limits,
            requests: HashMap::new(),
            pending: HashMap::new(),
            capture_store: CaptureStore::new(capture_limits)?,
            pending_completion: None,
            cancelled_captures: HashSet::new(),
            cancelled_capture_order: VecDeque::new(),
            highest_generation: 0,
            viewport_rows: viewport_rows.max(1),
            current_view: None,
        })
    }

    fn install_view(&mut self, view: CandidateView) {
        let selected = view
            .selected_index
            .map_or(0, |index| index as usize)
            .min(view.items.len().saturating_sub(1));
        self.current_view = Some(CachedView { view, selected });
    }

    fn navigate(&mut self, request_id: RequestId, generation: Generation, action: Navigation) {
        let Some(cached) = self.current_view.as_mut() else {
            return;
        };
        if cached.view.request_id != request_id || cached.view.generation != generation {
            return;
        }
        let last = cached.view.items.len().saturating_sub(1);
        cached.selected = match action {
            Navigation::Next => cached.selected.saturating_add(1).min(last),
            Navigation::Previous => cached.selected.saturating_sub(1),
            Navigation::PageDown => cached
                .selected
                .saturating_add((self.viewport_rows / 2).max(1))
                .min(last),
            Navigation::PageUp => cached
                .selected
                .saturating_sub((self.viewport_rows / 2).max(1)),
        };
    }

    fn current_window(&self) -> Option<WindowedView> {
        let cached = self.current_view.as_ref()?;
        let total = cached.view.items.len();
        let selected = cached.selected.min(total.saturating_sub(1));
        let rows = self.viewport_rows.min(total);
        let mut start = selected.saturating_add(1).saturating_sub(rows);
        start = start.min(total.saturating_sub(rows));
        let end = start.saturating_add(rows);
        let mut view = cached.view.clone();
        view.items = cached.view.items[start..end].to_vec();
        view.selected_index = (!view.items.is_empty()).then_some(
            u32::try_from(selected - start).expect("viewport selection index fits in u32"),
        );
        Some(WindowedView {
            view,
            total,
            start,
            selected_absolute: selected,
        })
    }

    async fn handle_shell<W>(
        &mut self,
        input: ShellInput,
        zle: &mut DaemonConnection,
        worker: &mut DaemonConnection,
        shell: &mut FramedWrite<W, ShellWireCodec>,
    ) -> Result<bool, BridgeError>
    where
        W: AsyncWrite + Unpin,
    {
        match input {
            ShellInput::Complete(request) => {
                self.send_completion(request, zle).await?;
            }
            ShellInput::Cancel(request_id, generation) => {
                let key = (request_id, generation);
                self.requests.remove(&key);
                self.pending.remove(&key);
                self.mark_capture_cancelled(key);
                self.capture_store.cancel(request_id, generation);
                if self.pending_completion.as_ref().is_some_and(|request| {
                    request.request_id == request_id && request.generation == generation
                }) {
                    self.pending_completion = None;
                }
                zle.send(ClientMessage::Cancel {
                    session_id: self.session_id,
                    request_id,
                    generation,
                })
                .await?;
            }
            ShellInput::Select(request_id, generation, item_id) => {
                zle.send(ClientMessage::Select(sense_protocol::SelectionRequest {
                    session_id: self.session_id,
                    request_id,
                    generation,
                    item_id,
                }))
                .await?;
            }
            ShellInput::Navigate(request_id, generation, action) => {
                self.navigate(request_id, generation, action);
                if let Some(window) = self.current_window() {
                    send_candidate_view(shell, window, &self.capture_store).await?;
                }
            }
            ShellInput::CaptureBegin(request_id, generation, backend) => {
                self.begin_capture(request_id, generation, backend)?;
            }
            ShellInput::Candidate(request_id, generation, candidate) => {
                self.push_candidate(request_id, generation, *candidate)?;
            }
            ShellInput::CandidateChunk(request_id, generation, candidates) => {
                for candidate in candidates {
                    self.push_candidate(request_id, generation, candidate)?;
                }
            }
            ShellInput::CaptureEnd(request_id, generation) => {
                for batch in self.end_capture(request_id, generation)? {
                    worker.send(ClientMessage::PublishCandidates(batch)).await?;
                }
            }
            ShellInput::Ping(nonce) => {
                zle.send(ClientMessage::Ping { nonce }).await?;
            }
            ShellInput::Goodbye => return Ok(true),
        }
        Ok(false)
    }

    fn mark_capture_cancelled(&mut self, key: RequestKey) {
        if self.cancelled_captures.insert(key) {
            self.cancelled_capture_order.push_back(key);
        }
        while self.cancelled_capture_order.len() > MAX_CANCELLED_CAPTURE_TOMBSTONES {
            if let Some(expired) = self.cancelled_capture_order.pop_front() {
                self.cancelled_captures.remove(&expired);
            }
        }
    }

    fn queue_completion(&mut self, request: CompletionRequest) {
        self.highest_generation = self.highest_generation.max(request.generation.0);
        if let Some(previous) = self.pending_completion.replace(request.clone()) {
            self.requests
                .remove(&(previous.request_id, previous.generation));
        }
        self.requests
            .insert((request.request_id, request.generation), request);
    }

    async fn dispatch_queued_completion(
        &mut self,
        zle: &mut DaemonConnection,
    ) -> Result<(), ProtocolError> {
        if let Some(request) = self.pending_completion.take() {
            tracing::trace!(
                request_id = request.request_id.0,
                generation = request.generation.0,
                "dispatching debounced completion"
            );
            zle.send(ClientMessage::Complete(request)).await?;
        }
        Ok(())
    }

    async fn send_completion(
        &mut self,
        request: CompletionRequest,
        zle: &mut DaemonConnection,
    ) -> Result<(), ProtocolError> {
        self.highest_generation = self.highest_generation.max(request.generation.0);
        let key = (request.request_id, request.generation);
        tracing::trace!(
            request_id = request.request_id.0,
            generation = request.generation.0,
            trigger = ?request.trigger,
            "dispatching completion"
        );
        self.requests.insert(key, request.clone());
        zle.send(ClientMessage::Complete(request)).await
    }

    fn begin_capture(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        backend: CaptureBackend,
    ) -> Result<(), BridgeError> {
        let key = (request_id, generation);
        if self.cancelled_captures.contains(&key) || self.is_stale_capture(key) {
            return Ok(());
        }
        if !self.requests.contains_key(&key) {
            return Err(BridgeError::UnknownRequest {
                request_id,
                generation,
            });
        }
        if self
            .pending
            .insert(
                key,
                PendingCapture {
                    backend,
                    matches: Vec::new(),
                    retained_bytes: 0,
                    dropped: 0,
                },
            )
            .is_some()
        {
            return Err(BridgeError::CaptureAlreadyStarted {
                request_id,
                generation,
            });
        }
        Ok(())
    }

    fn push_candidate(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        candidate: CapturedMatch,
    ) -> Result<(), BridgeError> {
        let key = (request_id, generation);
        if self.cancelled_captures.contains(&key) || self.is_stale_capture(key) {
            return Ok(());
        }
        let capture = self
            .pending
            .get_mut(&key)
            .ok_or(BridgeError::CaptureNotStarted {
                request_id,
                generation,
            })?;
        let candidate_bytes = super::capture_size(&candidate);
        if capture.matches.len() == self.capture_limits.max_candidates
            || capture.retained_bytes.saturating_add(candidate_bytes)
                > self.capture_limits.max_bytes
        {
            capture.dropped = capture.dropped.saturating_add(1);
        } else {
            capture.retained_bytes += candidate_bytes;
            capture.matches.push(candidate);
        }
        Ok(())
    }

    fn end_capture(
        &mut self,
        request_id: RequestId,
        generation: Generation,
    ) -> Result<Vec<CandidateBatch>, BridgeError> {
        let key = (request_id, generation);
        if self.cancelled_captures.remove(&key) {
            return Ok(Vec::new());
        }
        if self.is_stale_capture(key) {
            return Ok(Vec::new());
        }
        let capture = self
            .pending
            .remove(&key)
            .ok_or(BridgeError::CaptureNotStarted {
                request_id,
                generation,
            })?;
        let request = self.requests.get(&key).ok_or(BridgeError::UnknownRequest {
            request_id,
            generation,
        })?;
        let mut outcome = self
            .capture_store
            .install(request, capture.backend, capture.matches)?;
        if capture.dropped != 0 {
            outcome.dropped = outcome.dropped.saturating_add(capture.dropped);
            outcome.batch.is_incomplete = true;
        }
        split_candidate_batch(outcome.batch, self.daemon_frame_bytes)
    }

    fn is_stale_capture(&self, key: RequestKey) -> bool {
        !self.requests.contains_key(&key) && key.1.0 <= self.highest_generation
    }
}

fn split_candidate_batch(
    mut batch: CandidateBatch,
    max_frame_bytes: usize,
) -> Result<Vec<CandidateBatch>, BridgeError> {
    let final_flag = batch.is_final;
    let incomplete_flag = batch.is_incomplete;
    let items = std::mem::take(&mut batch.items);
    batch.is_final = false;
    batch.is_incomplete = false;

    if items.is_empty() {
        ensure_candidate_frame_fits(&batch, max_frame_bytes)?;
        batch.is_final = final_flag;
        batch.is_incomplete = incomplete_flag;
        return Ok(vec![batch]);
    }

    let base_size = candidate_frame_size(&batch)?;
    let mut chunks = Vec::new();
    let mut current = Vec::new();
    let mut estimated_size = base_size;
    for item in items {
        let item_size = rmp_serde::to_vec_named(&item)?.len().saturating_add(8);
        if !current.is_empty() && estimated_size.saturating_add(item_size) > max_frame_bytes {
            fit_candidate_chunk(&batch, current, max_frame_bytes, &mut chunks)?;
            current = Vec::new();
            estimated_size = base_size;
        }
        estimated_size = estimated_size.saturating_add(item_size);
        current.push(item);
    }
    if !current.is_empty() {
        fit_candidate_chunk(&batch, current, max_frame_bytes, &mut chunks)?;
    }
    let last = chunks
        .last_mut()
        .expect("a non-empty input creates a chunk");
    last.is_final = final_flag;
    last.is_incomplete = incomplete_flag;
    Ok(chunks)
}

fn fit_candidate_chunk(
    template: &CandidateBatch,
    items: Vec<CompletionItem>,
    max_frame_bytes: usize,
    output: &mut Vec<CandidateBatch>,
) -> Result<(), BridgeError> {
    let mut candidate = template.clone();
    candidate.items = items;
    let actual = candidate_frame_size(&candidate)?;
    if actual <= max_frame_bytes {
        output.push(candidate);
        return Ok(());
    }
    if candidate.items.len() == 1 {
        return Err(BridgeError::CandidateFrameTooLarge {
            item_id: candidate.items[0].id.0.clone(),
            actual,
            limit: max_frame_bytes,
        });
    }
    let right = candidate.items.split_off(candidate.items.len() / 2);
    fit_candidate_chunk(template, candidate.items, max_frame_bytes, output)?;
    fit_candidate_chunk(template, right, max_frame_bytes, output)
}

fn ensure_candidate_frame_fits(
    batch: &CandidateBatch,
    max_frame_bytes: usize,
) -> Result<(), BridgeError> {
    let actual = candidate_frame_size(batch)?;
    if actual <= max_frame_bytes {
        Ok(())
    } else {
        Err(BridgeError::CandidateFrameTooLarge {
            item_id: "<empty-batch>".into(),
            actual,
            limit: max_frame_bytes,
        })
    }
}

fn candidate_frame_size(batch: &CandidateBatch) -> Result<usize, rmp_serde::encode::Error> {
    rmp_serde::to_vec_named(&ClientMessage::PublishCandidates(batch.clone()))
        .map(|encoded| encoded.len())
}

fn bridge_state(
    config: &BridgeConfig,
    session_id: SessionId,
    daemon_frame_bytes: usize,
) -> Result<BridgeState, CaptureError> {
    BridgeState::new(
        session_id,
        daemon_frame_bytes,
        config.capture_limits,
        config.viewport_rows,
    )
}

/// Run a bridge over arbitrary asynchronous shell streams.
///
/// The bridge creates two authenticated daemon peers: the ZLE client owns the
/// session and the completion worker attaches to it. Keeping both connections
/// in one process avoids a subprocess per completion or per candidate.
///
/// # Errors
///
/// Returns on invalid shell frames, daemon protocol failures, stale capture
/// state, or an unexpected connection close.
pub async fn run_bridge<R, W>(
    config: BridgeConfig,
    shell_input: R,
    shell_output: W,
) -> Result<(), BridgeError>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    if config.daemon_frame_bytes == 0 {
        return Err(BridgeError::InvalidDaemonFrameSize);
    }
    let mut zle = connect_daemon(&config, PeerRole::ZleClient, None).await?;
    let zle_welcome = negotiated_session(&mut zle, "ZLE client").await?;
    let session_id = zle_welcome.session_id;
    let mut worker = connect_daemon(&config, PeerRole::CompletionWorker, Some(session_id)).await?;
    let worker_welcome = negotiated_session(&mut worker, "completion worker").await?;
    if worker_welcome.session_id != session_id {
        return Err(BridgeError::UnexpectedHandshake {
            role: "completion worker",
            message: "daemon attached the worker to a different session".into(),
        });
    }

    let input_codec = ShellWireCodec::new(config.shell_wire_limits)?;
    let output_codec = ShellWireCodec::new(config.shell_wire_limits)?;
    let mut shell_reader = FramedRead::new(shell_input, input_codec);
    let mut shell_writer = FramedWrite::new(shell_output, output_codec);
    shell_writer.set_backpressure_boundary(SHELL_OUTPUT_BACKPRESSURE_BYTES);
    let daemon_frame_bytes = config
        .daemon_frame_bytes
        .min(zle_welcome.max_frame_bytes as usize)
        .min(worker_welcome.max_frame_bytes as usize);
    let mut state = bridge_state(&config, session_id, daemon_frame_bytes)?;
    let mut debounce_timer = Box::pin(tokio::time::sleep(Duration::from_hours(24)));
    let mut debounce_armed = false;

    send_shell(
        &mut shell_writer,
        "ready",
        [
            session_id.0.to_string().into(),
            config.client_version.as_str().into(),
            ProtocolVersion::CURRENT.major.to_string().into(),
            ProtocolVersion::CURRENT.minor.to_string().into(),
        ],
    )
    .await?;
    for message in config.startup_messages.clone() {
        shell_writer.send(message).await?;
    }

    loop {
        tokio::select! {
            () = &mut debounce_timer, if debounce_armed => {
                debounce_armed = false;
                state.dispatch_queued_completion(&mut zle).await?;
            }
            shell_message = shell_reader.next() => {
                let Some(shell_message) = shell_message else {
                    send_goodbye(&mut zle, &mut worker).await;
                    return Ok(());
                };
                let input = parse_shell_message(&shell_message?, session_id)?;
                tracing::trace!(command = shell_input_name(&input), "received shell message");
                match input {
                    ShellInput::Complete(request)
                        if request.trigger == TriggerKind::Automatic
                            && !config.debounce.is_zero() =>
                    {
                        state.queue_completion(request);
                        debounce_timer.as_mut().reset(tokio::time::Instant::now() + config.debounce);
                        debounce_armed = true;
                    }
                    input => {
                        if state
                            .handle_shell(input, &mut zle, &mut worker, &mut shell_writer)
                            .await?
                        {
                            send_goodbye(&mut zle, &mut worker).await;
                            return Ok(());
                        }
                    }
                }
            }
            zle_message = zle.next() => {
                let Some(zle_message) = zle_message else {
                    return Err(BridgeError::DaemonClosed { role: "ZLE client" });
                };
                handle_zle_message(
                    zle_message?,
                    &mut shell_writer,
                    &mut state,
                ).await?;
            }
            worker_message = worker.next() => {
                let Some(worker_message) = worker_message else {
                    return Err(BridgeError::DaemonClosed { role: "completion worker" });
                };
                handle_worker_message(
                    worker_message?,
                    &mut shell_writer,
                    &mut state,
                ).await?;
            }
        }
    }
}

/// Run the bridge using standard input and output.
///
/// # Errors
///
/// Propagates [`run_bridge`] failures.
pub async fn run_stdio_bridge(config: BridgeConfig) -> Result<(), BridgeError> {
    run_bridge(config, tokio::io::stdin(), tokio::io::stdout()).await
}

/// Run the bridge over two pre-created private FIFOs.
///
/// `input_path` carries Zsh-to-worker messages and `output_path` carries
/// worker-to-Zsh messages. The Zsh client opens both with `cloexec` and unlinks
/// them immediately after the handshake.
///
/// # Errors
///
/// Rejects non-FIFO or group/other-accessible paths and propagates bridge
/// errors.
pub async fn run_fifo_bridge(
    config: BridgeConfig,
    input_path: PathBuf,
    output_path: PathBuf,
) -> Result<(), BridgeError> {
    validate_fifo(&input_path)?;
    validate_fifo(&output_path)?;
    let input = tokio::task::spawn_blocking({
        let input_path = input_path.clone();
        move || OpenOptions::new().read(true).open(input_path)
    })
    .await??;
    let output =
        tokio::task::spawn_blocking(move || OpenOptions::new().write(true).open(output_path))
            .await??;
    run_bridge(
        config,
        tokio::fs::File::from_std(input),
        tokio::fs::File::from_std(output),
    )
    .await
}

fn validate_fifo(path: &PathBuf) -> Result<(), BridgeError> {
    let metadata = fs::symlink_metadata(path)?;
    if !metadata.file_type().is_fifo() {
        return Err(BridgeError::InvalidFifo { path: path.clone() });
    }
    if metadata.permissions().mode() & 0o077 != 0 {
        return Err(BridgeError::InsecureFifo { path: path.clone() });
    }
    Ok(())
}

async fn connect_daemon(
    config: &BridgeConfig,
    role: PeerRole,
    attach_session: Option<SessionId>,
) -> Result<DaemonConnection, BridgeError> {
    let stream = UnixStream::connect(&config.socket_path).await?;
    let codec = MessagePackCodec::<ServerMessage, ClientMessage>::new(config.daemon_frame_bytes);
    let mut connection = Framed::new(stream, codec);
    connection
        .send(ClientMessage::Hello(ClientHello {
            protocol: ProtocolVersion::CURRENT,
            client_version: config.client_version.clone(),
            role,
            process_id: std::process::id(),
            zsh: config.zsh.clone(),
            attach_session,
        }))
        .await
        .map_err(BridgeError::from)?;
    Ok(connection)
}

async fn negotiated_session(
    connection: &mut DaemonConnection,
    role: &'static str,
) -> Result<ServerHello, BridgeError> {
    match connection.next().await {
        Some(Ok(ServerMessage::Welcome(welcome))) => Ok(welcome),
        Some(Ok(ServerMessage::Error { code, message, .. })) => {
            Err(BridgeError::HandshakeRejected {
                role,
                code,
                message,
            })
        }
        Some(Ok(message)) => Err(BridgeError::UnexpectedHandshake {
            role,
            message: server_message_name(&message).into(),
        }),
        Some(Err(error)) => Err(error.into()),
        None => Err(BridgeError::DaemonClosed { role }),
    }
}

async fn handle_zle_message<W>(
    message: ServerMessage,
    shell: &mut FramedWrite<W, ShellWireCodec>,
    state: &mut BridgeState,
) -> Result<(), BridgeError>
where
    W: AsyncWrite + Unpin,
{
    match message {
        ServerMessage::CandidateView(view) => {
            state.install_view(view);
            if let Some(window) = state.current_window() {
                send_candidate_view(shell, window, &state.capture_store).await?;
            }
        }
        ServerMessage::RequestStarted {
            request_id,
            generation,
        } => {
            send_request_event(shell, "request-started", request_id, generation, []).await?;
        }
        ServerMessage::RequestCancelled {
            request_id,
            generation,
        } => {
            let key = (request_id, generation);
            state.requests.remove(&key);
            state.pending.remove(&key);
            state.mark_capture_cancelled(key);
            state.capture_store.cancel(request_id, generation);
            send_request_event(shell, "request-cancelled", request_id, generation, []).await?;
        }
        ServerMessage::RequestFinished {
            request_id,
            generation,
            cancelled,
        } => {
            state.requests.remove(&(request_id, generation));
            state.pending.remove(&(request_id, generation));
            send_request_event(
                shell,
                "request-finished",
                request_id,
                generation,
                [bool_field(cancelled)],
            )
            .await?;
        }
        ServerMessage::Documentation {
            request_id,
            generation,
            item_id,
            documentation,
        } => {
            let (state, kind, value) = documentation_fields(documentation);
            send_request_event(
                shell,
                "documentation",
                request_id,
                generation,
                [item_id.0.into(), state.into(), kind.into(), value.into()],
            )
            .await?;
        }
        ServerMessage::Status { message } => {
            send_shell(shell, "status", [message.into()]).await?;
        }
        ServerMessage::Pong { nonce } => {
            send_shell(shell, "pong", [nonce.to_string().into()]).await?;
        }
        ServerMessage::Error {
            code,
            message,
            request_id,
        } => {
            send_shell(
                shell,
                "error",
                [
                    code.into(),
                    message.into(),
                    request_id.map_or_else(RawBytes::default, |id| id.0.to_string().into()),
                ],
            )
            .await?;
        }
        ServerMessage::SelectionAccepted(selection) => {
            send_request_event(
                shell,
                "selection-accepted",
                selection.request_id,
                selection.generation,
                [selection.item_id.0.into()],
            )
            .await?;
        }
        ServerMessage::Welcome(_)
        | ServerMessage::CompletionRequested(_)
        | ServerMessage::SelectionRequested(_)
        | ServerMessage::ResolveRequested(_)
        | ServerMessage::Candidates(_)
        | ServerMessage::Signature { .. }
        | ServerMessage::Diagnostics { .. }
        | ServerMessage::Preview { .. } => {}
    }
    Ok(())
}

async fn handle_worker_message<W>(
    message: ServerMessage,
    shell: &mut FramedWrite<W, ShellWireCodec>,
    state: &mut BridgeState,
) -> Result<(), BridgeError>
where
    W: AsyncWrite + Unpin,
{
    match message {
        ServerMessage::CompletionRequested(request) => {
            tracing::trace!(
                request_id = request.request_id.0,
                generation = request.generation.0,
                "requesting Zsh completion capture"
            );
            let key = (request.request_id, request.generation);
            state.requests.insert(key, request.clone());
            send_shell(
                shell,
                "capture-request",
                [
                    request.request_id.0.to_string().into(),
                    request.generation.0.to_string().into(),
                    request.buffer,
                    request.cursor.0.to_string().into(),
                ],
            )
            .await?;
        }
        ServerMessage::RequestCancelled {
            request_id,
            generation,
        } => {
            let key = (request_id, generation);
            state.requests.remove(&key);
            state.pending.remove(&key);
            state.mark_capture_cancelled(key);
            state.capture_store.cancel(request_id, generation);
        }
        ServerMessage::SelectionRequested(selection) => {
            match state.capture_store.acceptance_by_item(
                selection.request_id,
                selection.generation,
                &selection.item_id,
            ) {
                Ok(route) => send_acceptance(shell, route.clone()).await?,
                Err(error) => {
                    send_shell(
                        shell,
                        "error",
                        [
                            RawBytes::from("stale-selection"),
                            error.to_string().into(),
                            selection.request_id.0.to_string().into(),
                        ],
                    )
                    .await?;
                }
            }
        }
        ServerMessage::Error {
            code,
            message,
            request_id,
        } => {
            send_shell(
                shell,
                "error",
                [
                    code.into(),
                    message.into(),
                    request_id.map_or_else(RawBytes::default, |id| id.0.to_string().into()),
                ],
            )
            .await?;
        }
        ServerMessage::Welcome(_)
        | ServerMessage::CandidateView(_)
        | ServerMessage::ResolveRequested(_)
        | ServerMessage::RequestStarted { .. }
        | ServerMessage::Candidates(_)
        | ServerMessage::RequestFinished { .. }
        | ServerMessage::Documentation { .. }
        | ServerMessage::Signature { .. }
        | ServerMessage::Diagnostics { .. }
        | ServerMessage::Preview { .. }
        | ServerMessage::SelectionAccepted(_)
        | ServerMessage::Status { .. }
        | ServerMessage::Pong { .. } => {}
    }
    Ok(())
}

fn shell_input_name(input: &ShellInput) -> &'static str {
    match input {
        ShellInput::Complete(_) => "complete",
        ShellInput::Cancel(_, _) => "cancel",
        ShellInput::Select(_, _, _) => "select",
        ShellInput::Navigate(_, _, _) => "navigate",
        ShellInput::CaptureBegin(_, _, _) => "capture-begin",
        ShellInput::Candidate(_, _, _) => "candidate",
        ShellInput::CandidateChunk(_, _, _) => "command-candidates",
        ShellInput::CaptureEnd(_, _) => "capture-end",
        ShellInput::Ping(_) => "ping",
        ShellInput::Goodbye => "goodbye",
    }
}

async fn send_candidate_view<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    window: WindowedView,
    capture_store: &CaptureStore,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let view = window.view;
    let mut begin_fields = vec![
        view.session_id.0.to_string().into(),
        view.request_id.0.to_string().into(),
        view.generation.0.to_string().into(),
        view.revision.to_string().into(),
        view.selected_index
            .map_or_else(RawBytes::default, |index| index.to_string().into()),
        view.matched_before_limit.to_string().into(),
        bool_field(view.is_final),
        bool_field(view.is_incomplete),
        view.items.len().to_string().into(),
        window.total.to_string().into(),
        window.start.to_string().into(),
        window.selected_absolute.to_string().into(),
        view.sources_pending.len().to_string().into(),
    ];
    begin_fields.extend(
        view.sources_pending
            .iter()
            .map(|source| RawBytes::from(source.0.as_str())),
    );
    feed_shell(shell, "view-begin", begin_fields).await?;
    for items in view.items.chunks(VIEW_CHUNK_ITEMS) {
        feed_view_chunk(
            shell,
            view.request_id,
            view.generation,
            items,
            capture_store,
        )
        .await?;
    }
    feed_shell(
        shell,
        "view-end",
        [
            view.request_id.0.to_string().into(),
            view.generation.0.to_string().into(),
            view.revision.to_string().into(),
        ],
    )
    .await?;
    shell.flush().await
}

async fn feed_view_chunk<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    request_id: RequestId,
    generation: Generation,
    items: &[CompletionItem],
    capture_store: &CaptureStore,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let mut fields = Vec::with_capacity(3 + items.len() * VIEW_CHUNK_ITEM_FIELDS);
    fields.extend([
        request_id.0.to_string().into(),
        generation.0.to_string().into(),
        items.len().to_string().into(),
    ]);
    for item in items {
        let acceptance = capture_store
            .acceptance_by_item(request_id, generation, &item.id)
            .ok();
        fields.extend([
            item.id.0.as_str().into(),
            item.label.as_str().into(),
            completion_kind_name(item.kind).into(),
            optional_text(item.detail.as_deref()),
            item.group
                .as_ref()
                .map_or_else(RawBytes::default, |group| group.0.as_str().into()),
            acceptance.map_or_else(RawBytes::default, |route| {
                capture_backend_name(route.backend).into()
            }),
            acceptance.map_or_else(RawBytes::default, |route| route.backend_identity.clone()),
        ]);
    }
    feed_shell(shell, "view-chunk", fields).await
}

async fn send_acceptance<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    route: crate::AcceptanceRoute,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let metadata = route.insertion_metadata;
    let mut fields = vec![
        route.request_id.0.to_string().into(),
        route.generation.0.to_string().into(),
        capture_backend_name(route.backend).into(),
        route.ordinal.to_string().into(),
        route.backend_identity,
        route.insertion,
        route.flags.bits().to_string().into(),
        metadata.prefix,
        metadata.suffix,
        metadata.hidden_prefix,
        metadata.hidden_suffix,
        metadata.ignored_prefix,
        metadata.ignored_suffix,
        metadata.path_prefix,
        metadata.path_suffix,
        metadata.path_directory,
        metadata.removable_suffix_characters,
        metadata.suffix_removal_function,
        metadata.matcher_specs.len().to_string().into(),
    ];
    fields.extend(metadata.matcher_specs);
    send_shell(shell, "accept-zsh", fields).await
}

fn parse_shell_message(
    message: &ShellWireMessage,
    session_id: SessionId,
) -> Result<ShellInput, BridgeError> {
    let command = message.command.clone();
    let invalid = |reason: String| BridgeError::InvalidShellCommand {
        command: command.clone(),
        message: reason,
    };
    match message.command.as_str() {
        "complete" => parse_completion(&message.fields, session_id)
            .map(ShellInput::Complete)
            .map_err(invalid),
        "cancel" => {
            expect_fields(&message.fields, 2).map_err(invalid)?;
            Ok(ShellInput::Cancel(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
            ))
        }
        "select" => {
            expect_fields(&message.fields, 3).map_err(invalid)?;
            Ok(ShellInput::Select(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
                ItemId(parse_utf8(&message.fields[2], "item identifier").map_err(invalid)?),
            ))
        }
        "navigate" => {
            expect_fields(&message.fields, 3).map_err(invalid)?;
            let action =
                match parse_utf8_ref(&message.fields[2], "navigation action").map_err(invalid)? {
                    "next" => Navigation::Next,
                    "previous" => Navigation::Previous,
                    "page-down" => Navigation::PageDown,
                    "page-up" => Navigation::PageUp,
                    value => return Err(invalid(format!("unknown navigation action: {value}"))),
                };
            Ok(ShellInput::Navigate(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
                action,
            ))
        }
        "capture-begin" => {
            expect_fields(&message.fields, 3).map_err(invalid)?;
            Ok(ShellInput::CaptureBegin(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
                parse_capture_backend(&message.fields[2]).map_err(invalid)?,
            ))
        }
        "candidate" => parse_candidate(&message.fields)
            .map(|(request_id, generation, candidate)| {
                ShellInput::Candidate(request_id, generation, Box::new(candidate))
            })
            .map_err(invalid),
        "command-candidates" => parse_command_candidates(&message.fields)
            .map(|(request_id, generation, candidates)| {
                ShellInput::CandidateChunk(request_id, generation, candidates)
            })
            .map_err(invalid),
        "capture-end" => {
            expect_fields(&message.fields, 2).map_err(invalid)?;
            Ok(ShellInput::CaptureEnd(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
            ))
        }
        "ping" => {
            expect_fields(&message.fields, 1).map_err(invalid)?;
            Ok(ShellInput::Ping(
                parse_u64(&message.fields[0], "ping nonce").map_err(invalid)?,
            ))
        }
        "goodbye" => {
            expect_fields(&message.fields, 0).map_err(invalid)?;
            Ok(ShellInput::Goodbye)
        }
        _ => Err(invalid("unknown command".into())),
    }
}

fn parse_completion(
    fields: &[RawBytes],
    session_id: SessionId,
) -> Result<CompletionRequest, String> {
    if fields.len() < 11 {
        return Err("complete requires at least 11 fields".into());
    }
    let environment_count = parse_usize(&fields[10], "environment count")?;
    let expected = 11_usize
        .checked_add(
            environment_count
                .checked_mul(2)
                .ok_or("environment count overflow")?,
        )
        .ok_or("environment count overflow")?;
    expect_fields(fields, expected)?;
    let context_epoch = if fields[2].is_empty() {
        ContextEpoch::default()
    } else {
        let bytes: [u8; 32] = fields[2]
            .as_slice()
            .try_into()
            .map_err(|_| "context epoch must contain exactly 32 bytes")?;
        ContextEpoch(bytes)
    };
    let mut environment = BTreeMap::new();
    for pair in fields[11..].chunks_exact(2) {
        let key = parse_utf8(&pair[0], "environment key")?;
        if key.is_empty() || key.contains(['=', '\0']) {
            return Err("environment keys must be non-empty and cannot contain '=' or NUL".into());
        }
        environment.insert(key, pair[1].clone());
    }
    let request = CompletionRequest {
        session_id,
        request_id: parse_request_id(&fields[0])?,
        generation: parse_generation(&fields[1])?,
        context_epoch,
        buffer: fields[3].clone(),
        cursor: ByteOffset(parse_u32(&fields[4], "cursor")?),
        cwd: fields[5].clone(),
        keymap: parse_utf8(&fields[6], "keymap")?,
        terminal: TerminalDimensions {
            columns: parse_u16(&fields[7], "terminal columns")?,
            rows: parse_u16(&fields[8], "terminal rows")?,
        },
        trigger: parse_trigger(&fields[9])?,
        environment,
    };
    if !request.cursor_is_valid() {
        return Err("cursor is outside the command buffer".into());
    }
    Ok(request)
}

fn parse_candidate(fields: &[RawBytes]) -> Result<(RequestId, Generation, CapturedMatch), String> {
    if fields.len() < 27 {
        return Err("candidate requires at least 27 fields".into());
    }
    let matcher_count = parse_usize(&fields[26], "matcher count")?;
    let expected = 27_usize
        .checked_add(matcher_count)
        .ok_or("matcher count overflow")?;
    expect_fields(fields, expected)?;
    let group_name = optional_lossy(&fields[6]);
    let group_order = parse_u32(&fields[8], "group order")?;
    let group = group_name.map(|name| CapturedGroup {
        name,
        description: optional_lossy(&fields[7]),
        order: group_order,
    });
    let flags_bits = parse_u32(&fields[12], "Zsh match flags")?;
    let flags = ZshMatchFlags::from_bits(flags_bits)
        .ok_or_else(|| format!("unknown Zsh match flag bits: {flags_bits:#x}"))?;
    Ok((
        parse_request_id(&fields[0])?,
        parse_generation(&fields[1])?,
        CapturedMatch {
            insertion: fields[2].clone(),
            display: optional_lossy(&fields[3]),
            description: optional_lossy(&fields[4]),
            explanation: optional_lossy(&fields[5]),
            group,
            replace_range: TextRange::new(
                parse_u32(&fields[9], "replacement start")?,
                parse_u32(&fields[10], "replacement end")?,
            ),
            kind: parse_completion_kind(&fields[11])?,
            flags,
            backend_identity: fields[13].clone(),
            original_order: parse_u32(&fields[14], "original order")?,
            insertion_metadata: ZshInsertionMetadata {
                prefix: fields[15].clone(),
                suffix: fields[16].clone(),
                hidden_prefix: fields[17].clone(),
                hidden_suffix: fields[18].clone(),
                ignored_prefix: fields[19].clone(),
                ignored_suffix: fields[20].clone(),
                path_prefix: fields[21].clone(),
                path_suffix: fields[22].clone(),
                path_directory: fields[23].clone(),
                removable_suffix_characters: fields[24].clone(),
                suffix_removal_function: fields[25].clone(),
                matcher_specs: fields[27..].to_vec(),
            },
        },
    ))
}

// Command-name capture is the hottest continuous-completion path. Its Zsh
// insertion metadata is uniform across a chunk, so repeating the full generic
// 27-field representation for every command only burns time in the live shell.
// Decoding still produces the exact same CapturedMatch and acceptance route.
fn parse_command_candidates(
    fields: &[RawBytes],
) -> Result<(RequestId, Generation, Vec<CapturedMatch>), String> {
    const HEADER_FIELDS: usize = 10;
    const ITEM_FIELDS: usize = 2;
    if fields.len() < HEADER_FIELDS {
        return Err("command-candidates requires at least 10 fields".into());
    }
    let first_ordinal = parse_usize(&fields[8], "first candidate ordinal")?;
    if first_ordinal == 0 {
        return Err("first candidate ordinal must be one-based".into());
    }
    let count = parse_usize(&fields[9], "candidate count")?;
    let expected = HEADER_FIELDS
        .checked_add(
            count
                .checked_mul(ITEM_FIELDS)
                .ok_or("candidate count overflow")?,
        )
        .ok_or("candidate count overflow")?;
    expect_fields(fields, expected)?;

    let request_id = parse_request_id(&fields[0])?;
    let generation = parse_generation(&fields[1])?;
    let replace_range = TextRange::new(
        parse_u32(&fields[2], "replacement start")?,
        parse_u32(&fields[3], "replacement end")?,
    );
    let insertion_metadata = ZshInsertionMetadata {
        prefix: fields[4].clone(),
        suffix: fields[5].clone(),
        hidden_prefix: fields[6].clone(),
        hidden_suffix: fields[7].clone(),
        ..ZshInsertionMetadata::default()
    };

    let mut candidates = Vec::with_capacity(count);
    for (offset, item) in fields[HEADER_FIELDS..]
        .chunks_exact(ITEM_FIELDS)
        .enumerate()
    {
        let kind = parse_completion_kind(&item[1])?;
        let (description, group_name, explanation) = command_metadata(kind);
        let ordinal = first_ordinal
            .checked_add(offset)
            .ok_or("candidate ordinal overflow")?;
        let original_order =
            u32::try_from(ordinal - 1).map_err(|_| "candidate ordinal exceeds u32".to_string())?;
        candidates.push(CapturedMatch {
            insertion: item[0].clone(),
            display: optional_lossy(&item[0]),
            description: Some(description.into()),
            explanation: Some(explanation.into()),
            group: Some(CapturedGroup {
                name: group_name.into(),
                description: Some(explanation.into()),
                order: 0,
            }),
            replace_range,
            kind,
            flags: ZshMatchFlags::empty(),
            insertion_metadata: insertion_metadata.clone(),
            backend_identity: ordinal.to_string().into(),
            original_order,
        });
    }
    Ok((request_id, generation, candidates))
}

fn command_metadata(kind: CompletionKind) -> (&'static str, &'static str, &'static str) {
    match kind {
        CompletionKind::Alias => ("shell alias", "aliases", "shell aliases"),
        CompletionKind::Function => ("shell function", "functions", "shell functions"),
        CompletionKind::Builtin => ("builtin command", "builtins", "builtin commands"),
        CompletionKind::Command => ("external command", "external-commands", "external commands"),
        _ => ("reserved word", "reserved-words", "reserved words"),
    }
}

async fn send_shell<W, I>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    command: &str,
    fields: I,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
    I: IntoIterator<Item = RawBytes>,
{
    shell
        .send(ShellWireMessage::new(command, fields.into_iter().collect()))
        .await
}

async fn feed_shell<W, I>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    command: &str,
    fields: I,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
    I: IntoIterator<Item = RawBytes>,
{
    shell
        .feed(ShellWireMessage::new(command, fields.into_iter().collect()))
        .await
}

async fn send_request_event<W, I>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    command: &str,
    request_id: RequestId,
    generation: Generation,
    extra: I,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
    I: IntoIterator<Item = RawBytes>,
{
    let mut fields = vec![
        request_id.0.to_string().into(),
        generation.0.to_string().into(),
    ];
    fields.extend(extra);
    send_shell(shell, command, fields).await
}

async fn send_goodbye(zle: &mut DaemonConnection, worker: &mut DaemonConnection) {
    let _ = zle.send(ClientMessage::Goodbye).await;
    let _ = worker.send(ClientMessage::Goodbye).await;
}

fn parse_request_id(field: &RawBytes) -> Result<RequestId, String> {
    parse_u64(field, "request identifier").map(RequestId)
}

fn parse_generation(field: &RawBytes) -> Result<Generation, String> {
    parse_u64(field, "generation").map(Generation)
}

fn parse_u64(field: &RawBytes, name: &str) -> Result<u64, String> {
    canonical_decimal(field, name)?
        .parse()
        .map_err(|_| format!("{name} must be an unsigned decimal integer"))
}

fn parse_u32(field: &RawBytes, name: &str) -> Result<u32, String> {
    canonical_decimal(field, name)?
        .parse()
        .map_err(|_| format!("{name} must be an unsigned 32-bit decimal integer"))
}

fn parse_u16(field: &RawBytes, name: &str) -> Result<u16, String> {
    canonical_decimal(field, name)?
        .parse()
        .map_err(|_| format!("{name} must be an unsigned 16-bit decimal integer"))
}

fn parse_usize(field: &RawBytes, name: &str) -> Result<usize, String> {
    canonical_decimal(field, name)?
        .parse()
        .map_err(|_| format!("{name} must be an unsigned decimal integer"))
}

fn canonical_decimal<'a>(field: &'a RawBytes, name: &str) -> Result<&'a str, String> {
    let value = parse_utf8_ref(field, name)?;
    if value.is_empty()
        || !value.bytes().all(|byte| byte.is_ascii_digit())
        || (value.len() > 1 && value.starts_with('0'))
    {
        Err(format!(
            "{name} must be a canonical unsigned decimal integer"
        ))
    } else {
        Ok(value)
    }
}

fn parse_utf8(field: &RawBytes, name: &str) -> Result<String, String> {
    parse_utf8_ref(field, name).map(ToOwned::to_owned)
}

fn parse_utf8_ref<'a>(field: &'a RawBytes, name: &str) -> Result<&'a str, String> {
    std::str::from_utf8(field.as_slice()).map_err(|_| format!("{name} must be valid UTF-8"))
}

fn expect_fields(fields: &[RawBytes], expected: usize) -> Result<(), String> {
    if fields.len() == expected {
        Ok(())
    } else {
        Err(format!(
            "expected {expected} fields, received {}",
            fields.len()
        ))
    }
}

fn parse_trigger(field: &RawBytes) -> Result<TriggerKind, String> {
    match parse_utf8_ref(field, "trigger")? {
        "automatic" => Ok(TriggerKind::Automatic),
        "manual" => Ok(TriggerKind::Manual),
        "trigger-character" => Ok(TriggerKind::TriggerCharacter),
        "after-accept" => Ok(TriggerKind::AfterAccept),
        "incomplete-refresh" => Ok(TriggerKind::IncompleteRefresh),
        value => Err(format!("unknown trigger kind: {value}")),
    }
}

fn parse_capture_backend(field: &RawBytes) -> Result<CaptureBackend, String> {
    match parse_utf8_ref(field, "capture backend")? {
        "native" => Ok(CaptureBackend::Native),
        "portable" => Ok(CaptureBackend::Portable),
        value => Err(format!("unknown capture backend: {value}")),
    }
}

fn parse_completion_kind(field: &RawBytes) -> Result<CompletionKind, String> {
    match parse_utf8_ref(field, "completion kind")? {
        "text" => Ok(CompletionKind::Text),
        "command" => Ok(CompletionKind::Command),
        "alias" => Ok(CompletionKind::Alias),
        "builtin" => Ok(CompletionKind::Builtin),
        "function" => Ok(CompletionKind::Function),
        "subcommand" => Ok(CompletionKind::Subcommand),
        "option" => Ok(CompletionKind::Option),
        "option-value" => Ok(CompletionKind::OptionValue),
        "variable" => Ok(CompletionKind::Variable),
        "file" => Ok(CompletionKind::File),
        "directory" => Ok(CompletionKind::Directory),
        "symlink" => Ok(CompletionKind::Symlink),
        "user" => Ok(CompletionKind::User),
        "host" => Ok(CompletionKind::Host),
        "process" => Ok(CompletionKind::Process),
        "job" => Ok(CompletionKind::Job),
        "git-branch" => Ok(CompletionKind::GitBranch),
        "git-tag" => Ok(CompletionKind::GitTag),
        "git-commit" => Ok(CompletionKind::GitCommit),
        "service" => Ok(CompletionKind::Service),
        "container" => Ok(CompletionKind::Container),
        "image" => Ok(CompletionKind::Image),
        "package" => Ok(CompletionKind::Package),
        "history" => Ok(CompletionKind::History),
        "snippet" => Ok(CompletionKind::Snippet),
        "action" => Ok(CompletionKind::Action),
        value => Err(format!("unknown completion kind: {value}")),
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
        CompletionKind::History => "history",
        CompletionKind::Snippet => "snippet",
        CompletionKind::Action => "action",
    }
}

const fn capture_backend_name(backend: CaptureBackend) -> &'static str {
    match backend {
        CaptureBackend::Native => "native",
        CaptureBackend::Portable => "portable",
    }
}

fn optional_lossy(field: &RawBytes) -> Option<String> {
    (!field.is_empty()).then(|| field.display_lossy())
}

fn optional_text(value: Option<&str>) -> RawBytes {
    value.map_or_else(RawBytes::default, Into::into)
}

fn bool_field(value: bool) -> RawBytes {
    RawBytes::from(if value { "1" } else { "0" })
}

fn documentation_fields(
    documentation: sense_model::DocumentationState,
) -> (&'static str, &'static str, String) {
    match documentation {
        sense_model::DocumentationState::None => ("none", "", String::new()),
        sense_model::DocumentationState::Unresolved => ("unresolved", "", String::new()),
        sense_model::DocumentationState::Resolved(content) => (
            "resolved",
            match content.kind {
                sense_model::MarkupKind::PlainText => "plain-text",
                sense_model::MarkupKind::Markdown => "markdown",
            },
            content.value,
        ),
    }
}

fn server_message_name(message: &ServerMessage) -> &'static str {
    match message {
        ServerMessage::Welcome(_) => "welcome",
        ServerMessage::CompletionRequested(_) => "completion-requested",
        ServerMessage::RequestCancelled { .. } => "request-cancelled",
        ServerMessage::SelectionRequested(_) => "selection-requested",
        ServerMessage::ResolveRequested(_) => "resolve-requested",
        ServerMessage::RequestStarted { .. } => "request-started",
        ServerMessage::Candidates(_) => "candidates",
        ServerMessage::CandidateView(_) => "candidate-view",
        ServerMessage::RequestFinished { .. } => "request-finished",
        ServerMessage::Documentation { .. } => "documentation",
        ServerMessage::Signature { .. } => "signature",
        ServerMessage::Diagnostics { .. } => "diagnostics",
        ServerMessage::Preview { .. } => "preview",
        ServerMessage::SelectionAccepted(_) => "selection-accepted",
        ServerMessage::Status { .. } => "status",
        ServerMessage::Pong { .. } => "pong",
        ServerMessage::Error { .. } => "error",
    }
}

#[cfg(test)]
mod tests {
    use sense_model::{SourceId, TextEdit};

    use super::*;

    fn batch(item_count: usize) -> CandidateBatch {
        CandidateBatch {
            session_id: SessionId::new(),
            request_id: RequestId(1),
            generation: Generation(2),
            source: SourceId("zsh".into()),
            items: (0..item_count)
                .map(|index| {
                    let label = format!("candidate-{index}-{}", "x".repeat(96));
                    CompletionItem::plain(
                        format!("item-{index}"),
                        "zsh",
                        label.clone(),
                        TextEdit::new(TextRange::new(0, 0), label),
                    )
                })
                .collect(),
            is_final: true,
            is_incomplete: true,
        }
    }

    #[test]
    fn candidate_batches_are_split_under_the_negotiated_frame_limit() {
        let original = batch(40);
        let expected_ids: Vec<_> = original.items.iter().map(|item| item.id.clone()).collect();
        let chunks = split_candidate_batch(original, 2_000).unwrap();
        assert!(chunks.len() > 1);
        for (index, chunk) in chunks.iter().enumerate() {
            assert!(candidate_frame_size(chunk).unwrap() <= 2_000);
            assert_eq!(chunk.is_final, index == chunks.len() - 1);
            assert_eq!(chunk.is_incomplete, index == chunks.len() - 1);
        }
        let actual_ids: Vec<_> = chunks
            .iter()
            .flat_map(|chunk| chunk.items.iter().map(|item| item.id.clone()))
            .collect();
        assert_eq!(actual_ids, expected_ids);
    }

    #[test]
    fn one_candidate_larger_than_a_frame_is_rejected() {
        assert!(matches!(
            split_candidate_batch(batch(1), 32),
            Err(BridgeError::CandidateFrameTooLarge { .. })
        ));
    }

    #[test]
    fn delayed_capture_is_ignored_after_its_cancellation_tombstone_expires() {
        let mut state = BridgeState::new(
            SessionId::new(),
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            10,
        )
        .unwrap();
        state.highest_generation = 400;

        let delayed_key = (RequestId(45), Generation(45));
        assert!(!state.cancelled_captures.contains(&delayed_key));
        state
            .begin_capture(delayed_key.0, delayed_key.1, CaptureBackend::Portable)
            .unwrap();
        assert!(
            state
                .end_capture(delayed_key.0, delayed_key.1)
                .unwrap()
                .is_empty()
        );

        let future = (RequestId(401), Generation(401));
        assert!(matches!(
            state.begin_capture(future.0, future.1, CaptureBackend::Portable),
            Err(BridgeError::UnknownRequest { .. })
        ));
    }

    #[test]
    fn compact_command_chunk_preserves_capture_and_acceptance_fields() {
        let fields: Vec<RawBytes> = [
            "7", "11", "0", "1", "c", "", "", "", "4", "1", "cargo", "command",
        ]
        .into_iter()
        .map(Into::into)
        .collect();
        let (request, generation, candidates) = parse_command_candidates(&fields).unwrap();
        assert_eq!(request, RequestId(7));
        assert_eq!(generation, Generation(11));
        assert_eq!(candidates.len(), 1);
        let candidate = &candidates[0];
        assert_eq!(candidate.insertion.as_slice(), b"cargo");
        assert_eq!(candidate.description.as_deref(), Some("external command"));
        assert_eq!(candidate.kind, CompletionKind::Command);
        assert_eq!(candidate.replace_range, TextRange::new(0, 1));
        assert_eq!(candidate.insertion_metadata.prefix.as_slice(), b"c");
        assert_eq!(candidate.backend_identity.as_slice(), b"4");
        assert_eq!(candidate.original_order, 3);
    }
}
