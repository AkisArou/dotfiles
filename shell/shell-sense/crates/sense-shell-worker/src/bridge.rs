//! Persistent bridge between one interactive shell process and the daemon.

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs::{self, OpenOptions};
use std::io;
use std::io::SeekFrom;
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::OpenOptionsExt;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::path::PathBuf;
use std::time::Duration;

use bytes::BytesMut;
use futures_util::{SinkExt, StreamExt};
use sense_model::{
    ByteOffset, CompletionItem, CompletionKind, CompletionRequest, Confidence, ContextEpoch,
    Generation, GhostText, ItemId, NativeCommandContext, NativeShell, RawBytes, RequestId,
    SessionId, TerminalDimensions, TextEdit, TextRange, TriggerKind,
};
use sense_present::{
    DocumentationLine, DocumentationPanel, DocumentationPlacement,
    DocumentationPlacementPreference, PresentationRequest,
};
use sense_protocol::{
    CandidateBatch, CandidateView, ClientHello, ClientMessage, MessagePackCodec, PeerRole,
    ProtocolError, ProtocolVersion, ResolveRequest, ServerHello, ServerMessage, ShellIdentity,
};
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeekExt, AsyncWrite, AsyncWriteExt};
use tokio::net::UnixStream;
use tokio_util::codec::{Decoder, Framed, FramedRead, FramedWrite};
use unicode_width::UnicodeWidthStr;

use crate::{
    CaptureError, CaptureLimits, CaptureStore, CapturedGroup, CapturedMatch, ShellAcceptanceRoute,
    ShellCaptureStore, ShellCapturedMatch, ShellWireCodec, ShellWireError, ShellWireLimits,
    ShellWireMessage, ZshInsertionMetadata, ZshMatchFlags,
};

type DaemonConnection = Framed<UnixStream, MessagePackCodec<ServerMessage, ClientMessage>>;
type RequestKey = (RequestId, Generation);

const MAX_CANCELLED_CAPTURE_TOMBSTONES: usize = 256;
const MAX_NATIVE_CONTEXT_WORDS: usize = 4096;
const MAX_NATIVE_CONTEXT_BYTES: usize = 1024 * 1024;
const VIEW_CHUNK_ITEM_FIELDS: usize = 11;
// 3 envelope fields + (11 * 11) item fields = 124, below the default and
// shell-side 128-field wire limit.
const VIEW_CHUNK_ITEMS: usize = 11;
const SHELL_OUTPUT_BACKPRESSURE_BYTES: usize = 256 * 1024;

#[derive(Debug, Clone)]
pub struct BridgeConfig {
    pub socket_path: PathBuf,
    pub daemon_frame_bytes: usize,
    pub shell_wire_limits: ShellWireLimits,
    pub capture_limits: CaptureLimits,
    pub debounce: Duration,
    pub viewport_rows: usize,
    pub ghost_text: GhostTextPolicy,
    pub documentation: DocumentationPolicy,
    pub startup_messages: Vec<ShellWireMessage>,
    pub client_version: String,
    pub shell: ShellIdentity,
    pub shell_process_id: u32,
}

impl BridgeConfig {
    #[must_use]
    pub fn new(socket_path: impl Into<PathBuf>, shell: ShellIdentity) -> Self {
        Self {
            socket_path: socket_path.into(),
            daemon_frame_bytes: sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            shell_wire_limits: ShellWireLimits::default(),
            capture_limits: CaptureLimits::default(),
            debounce: Duration::from_millis(15),
            viewport_rows: 20,
            ghost_text: GhostTextPolicy::default(),
            documentation: DocumentationPolicy::default(),
            startup_messages: Vec::new(),
            client_version: env!("CARGO_PKG_VERSION").into(),
            shell,
            shell_process_id: std::process::id(),
        }
    }
}

/// Presentation policy for completion-derived ghost text.
///
/// The worker derives only a display-only suffix from an authoritative ranked
/// completion. Acceptance still follows the candidate's normal Zsh route.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GhostTextPolicy {
    pub enabled: bool,
    pub minimum_confidence: f32,
}

impl Default for GhostTextPolicy {
    fn default() -> Self {
        Self {
            enabled: true,
            minimum_confidence: 0.82,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentationActivation {
    Disabled,
    Automatic,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DocumentationPolicy {
    pub activation: DocumentationActivation,
    pub resolve_delay: Duration,
    pub layout: DocumentationLayoutPolicy,
    pub menu: MenuLayoutPolicy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DocumentationLayoutPolicy {
    pub placement: DocumentationPlacementPreference,
    pub side_min_columns: u16,
    pub width_ratio: f32,
    pub max_rows: u16,
    pub render_markdown: bool,
    pub padding: u16,
    pub bordered: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MenuLayoutPolicy {
    pub menu_min_width: u16,
    pub menu_max_width: u16,
    pub menu_max_rows: u16,
    pub menu_chrome_cells: u16,
    pub scrollbar: bool,
    pub descriptions: bool,
}

impl DocumentationPolicy {
    const fn enabled(self) -> bool {
        matches!(self.activation, DocumentationActivation::Automatic)
    }
}

impl Default for DocumentationPolicy {
    fn default() -> Self {
        Self {
            activation: DocumentationActivation::Automatic,
            resolve_delay: Duration::from_millis(80),
            layout: DocumentationLayoutPolicy {
                placement: DocumentationPlacementPreference::Auto,
                side_min_columns: 100,
                width_ratio: 0.45,
                max_rows: 14,
                render_markdown: true,
                padding: 1,
                bordered: false,
            },
            menu: MenuLayoutPolicy {
                menu_min_width: 24,
                menu_max_width: 140,
                menu_max_rows: 10,
                menu_chrome_cells: 4,
                scrollbar: true,
                descriptions: true,
            },
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
    #[error("{path} is not a regular mailbox file")]
    InvalidMailbox { path: PathBuf },
    #[error("mailbox {path} is not private and owned by the current user")]
    InsecureMailbox { path: PathBuf },
    #[error("shell process identifier {0} exceeds the platform process-id range")]
    InvalidShellProcessId(u32),
    #[error("could not notify the owning shell: {0}")]
    Signal(nix::errno::Errno),
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
    #[error("native context already exists for request {request_id:?} generation {generation:?}")]
    ContextAlreadyStarted {
        request_id: RequestId,
        generation: Generation,
    },
    #[error("native context was not started for request {request_id:?} generation {generation:?}")]
    ContextNotStarted {
        request_id: RequestId,
        generation: Generation,
    },
    #[error("native context exceeded its word or byte bounds")]
    ContextLimit,
    #[error("native context chunks must be contiguous and match the declared word count")]
    InvalidContextChunk,
    #[error(
        "candidate capture references an unknown request {request_id:?} generation {generation:?}"
    )]
    UnknownRequest {
        request_id: RequestId,
        generation: Generation,
    },
    #[error("{capture} capture is invalid for the active {shell:?} session")]
    WrongShellCapture {
        capture: &'static str,
        shell: NativeShell,
    },
}

#[derive(Debug)]
struct PendingCapture {
    matches: PendingMatches,
    retained_bytes: usize,
    dropped: usize,
}

#[derive(Debug)]
struct PendingNativeContext {
    current_word: Option<u32>,
    expected_words: usize,
    retained_bytes: usize,
    words: Vec<RawBytes>,
}

#[derive(Debug)]
enum PendingMatches {
    Zsh(Vec<CapturedMatch>),
    Shell(Vec<ShellCapturedMatch>),
}

#[derive(Debug)]
enum ShellInput {
    Complete(CompletionRequest),
    Cancel(RequestId, Generation),
    Select(RequestId, Generation, ItemId),
    SelectionFinished(RequestId, Generation, ItemId, bool),
    Navigate(RequestId, Generation, Navigation),
    ZshCaptureBegin(RequestId, Generation),
    ZshCandidate(RequestId, Generation, Box<CapturedMatch>),
    ZshCandidateChunk(RequestId, Generation, Vec<CapturedMatch>),
    ShellCaptureBegin(RequestId, Generation),
    ShellCandidate(RequestId, Generation, Box<ShellCapturedMatch>),
    ContextBegin(RequestId, Generation, Option<u32>, usize),
    ContextChunk(RequestId, Generation, usize, Vec<RawBytes>),
    ContextEnd(RequestId, Generation),
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

#[derive(Debug, Clone)]
enum Acceptance {
    Zsh(Box<crate::AcceptanceRoute>),
    Shell(ShellAcceptanceRoute),
}

#[derive(Debug)]
struct CachedView {
    view: CandidateView,
    selected: usize,
    max_label_cells: usize,
    max_described_cells: usize,
    ghost_texts: Vec<Option<GhostText>>,
    terminal: TerminalDimensions,
}

#[derive(Debug)]
struct WindowedView {
    view: CandidateView,
    total: usize,
    start: usize,
    selected_absolute: usize,
    max_label_cells: usize,
    max_described_cells: usize,
    ghost_texts: Vec<Option<GhostText>>,
    terminal: TerminalDimensions,
}

#[derive(Debug)]
struct BridgeState {
    shell: NativeShell,
    session_id: SessionId,
    daemon_frame_bytes: usize,
    capture_limits: CaptureLimits,
    requests: HashMap<(RequestId, Generation), CompletionRequest>,
    pending: HashMap<(RequestId, Generation), PendingCapture>,
    pending_contexts: HashMap<(RequestId, Generation), PendingNativeContext>,
    capture_store: CaptureStore,
    shell_capture_store: Option<ShellCaptureStore>,
    pending_completion: Option<CompletionRequest>,
    cancelled_captures: HashSet<RequestKey>,
    cancelled_capture_order: VecDeque<RequestKey>,
    highest_generation: u64,
    viewport_rows: usize,
    ghost_text: GhostTextPolicy,
    documentation: DocumentationPolicy,
    current_view: Option<CachedView>,
}

impl BridgeState {
    fn new(
        shell: NativeShell,
        session_id: SessionId,
        daemon_frame_bytes: usize,
        capture_limits: CaptureLimits,
        viewport_rows: usize,
        ghost_text: GhostTextPolicy,
        documentation: DocumentationPolicy,
    ) -> Result<Self, CaptureError> {
        Ok(Self {
            shell,
            session_id,
            daemon_frame_bytes,
            capture_limits,
            requests: HashMap::new(),
            pending: HashMap::new(),
            pending_contexts: HashMap::new(),
            capture_store: CaptureStore::new(capture_limits)?,
            shell_capture_store: if shell == NativeShell::Zsh {
                None
            } else {
                Some(ShellCaptureStore::new(shell, capture_limits)?)
            },
            pending_completion: None,
            cancelled_captures: HashSet::new(),
            cancelled_capture_order: VecDeque::new(),
            highest_generation: 0,
            viewport_rows: viewport_rows.max(1),
            ghost_text,
            documentation,
            current_view: None,
        })
    }

    fn install_view(&mut self, view: CandidateView) {
        let request = self.requests.get(&(view.request_id, view.generation));
        let terminal = request.map_or_else(TerminalDimensions::default, |request| request.terminal);
        let prefix_matches = view
            .items
            .iter()
            .filter(|item| {
                item.match_result
                    .as_ref()
                    .is_some_and(|matched| matched.prefix)
            })
            .count();
        let ghost_view_is_complete = view.is_final
            && !view.is_incomplete
            && usize::try_from(view.matched_before_limit)
                .is_ok_and(|matched| matched == view.items.len());
        let ghost_texts = view
            .items
            .iter()
            .map(|item| {
                request.and_then(|request| {
                    completion_ghost_text(
                        request,
                        item,
                        self.ghost_text,
                        ghost_view_is_complete && prefix_matches == 1,
                    )
                })
            })
            .collect();
        let selected = view
            .selected_index
            .map_or(0, |index| index as usize)
            .min(view.items.len().saturating_sub(1));
        let max_label_cells = view
            .items
            .iter()
            .map(|item| UnicodeWidthStr::width(item.label.as_str()))
            .max()
            .unwrap_or(0);
        let max_described_cells = view
            .items
            .iter()
            .map(|item| {
                let label_cells = UnicodeWidthStr::width(item.label.as_str());
                item.detail
                    .as_deref()
                    .filter(|detail| !detail.is_empty())
                    .map_or(label_cells, |detail| {
                        label_cells + 2 + UnicodeWidthStr::width(detail)
                    })
            })
            .max()
            .unwrap_or(0);
        self.current_view = Some(CachedView {
            view,
            selected,
            max_label_cells,
            max_described_cells,
            ghost_texts,
            terminal,
        });
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
            max_label_cells: cached.max_label_cells,
            max_described_cells: cached.max_described_cells,
            ghost_texts: cached.ghost_texts[start..end].to_vec(),
            terminal: cached.terminal,
        })
    }

    fn documentation_resolve_target(&self) -> Option<ResolveRequest> {
        if !self.documentation.enabled() {
            return None;
        }
        let cached = self.current_view.as_ref()?;
        if cached.view.generation.0 != self.highest_generation {
            return None;
        }
        let item = cached.view.items.get(cached.selected)?;
        (matches!(
            item.documentation,
            sense_model::DocumentationState::Unresolved
        ) || item
            .capabilities
            .contains(sense_model::ItemCapabilities::RESOLVE_DOCUMENTATION))
        .then(|| ResolveRequest {
            session_id: self.session_id,
            request_id: cached.view.request_id,
            generation: cached.view.generation,
            item_id: item.id.clone(),
        })
    }

    fn install_documentation(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        item_id: &ItemId,
        documentation: sense_model::DocumentationState,
    ) -> bool {
        let Some(cached) = self.current_view.as_mut().filter(|cached| {
            cached.view.request_id == request_id && cached.view.generation == generation
        }) else {
            return false;
        };
        let Some((index, item)) = cached
            .view
            .items
            .iter_mut()
            .enumerate()
            .find(|(_, item)| &item.id == item_id)
        else {
            return false;
        };
        item.documentation = documentation;
        item.capabilities
            .remove(sense_model::ItemCapabilities::RESOLVE_DOCUMENTATION);
        index == cached.selected
    }

    async fn handle_shell<W>(
        &mut self,
        input: ShellInput,
        client: &mut DaemonConnection,
        worker: &mut DaemonConnection,
        shell: &mut FramedWrite<W, ShellWireCodec>,
    ) -> Result<bool, BridgeError>
    where
        W: AsyncWrite + Unpin,
    {
        match input {
            ShellInput::Complete(request) => {
                self.send_completion(request, client).await?;
            }
            ShellInput::Cancel(request_id, generation) => {
                self.invalidate_request(request_id, generation);
                client
                    .send(ClientMessage::Cancel {
                        session_id: self.session_id,
                        request_id,
                        generation,
                    })
                    .await?;
            }
            ShellInput::Select(request_id, generation, item_id) => {
                client
                    .send(ClientMessage::Select(sense_protocol::SelectionRequest {
                        session_id: self.session_id,
                        request_id,
                        generation,
                        item_id,
                    }))
                    .await?;
            }
            ShellInput::SelectionFinished(request_id, generation, item_id, applied) => {
                client
                    .send(selection_result_message(
                        self.session_id,
                        request_id,
                        generation,
                        item_id,
                        applied,
                    ))
                    .await?;
            }
            ShellInput::Navigate(request_id, generation, action) => {
                self.navigate(request_id, generation, action);
                if let Some(window) = self.current_window() {
                    send_candidate_view(
                        shell,
                        window,
                        &self.capture_store,
                        self.shell_capture_store.as_ref(),
                        self.documentation,
                    )
                    .await?;
                }
            }
            ShellInput::ZshCaptureBegin(request_id, generation) => {
                self.begin_zsh_capture(request_id, generation)?;
            }
            ShellInput::ZshCandidate(request_id, generation, candidate) => {
                self.push_zsh_candidate(request_id, generation, *candidate)?;
            }
            ShellInput::ZshCandidateChunk(request_id, generation, candidates) => {
                for candidate in candidates {
                    self.push_zsh_candidate(request_id, generation, candidate)?;
                }
            }
            ShellInput::ShellCaptureBegin(request_id, generation) => {
                self.begin_shell_capture(request_id, generation)?;
            }
            ShellInput::ShellCandidate(request_id, generation, candidate) => {
                self.push_shell_candidate(request_id, generation, *candidate)?;
            }
            ShellInput::ContextBegin(request_id, generation, current_word, expected_words) => {
                self.begin_native_context(request_id, generation, current_word, expected_words)?;
            }
            ShellInput::ContextChunk(request_id, generation, start, words) => {
                self.push_native_context(request_id, generation, start, words)?;
            }
            ShellInput::ContextEnd(request_id, generation) => {
                let context = self.end_native_context(request_id, generation)?;
                worker
                    .send(ClientMessage::PublishNativeContext(
                        sense_protocol::NativeContextPublication {
                            session_id: self.session_id,
                            request_id,
                            generation,
                            context,
                        },
                    ))
                    .await?;
            }
            ShellInput::CaptureEnd(request_id, generation) => {
                for batch in self.end_capture(request_id, generation)? {
                    worker.send(ClientMessage::PublishCandidates(batch)).await?;
                }
            }
            ShellInput::Ping(nonce) => {
                client.send(ClientMessage::Ping { nonce }).await?;
            }
            ShellInput::Goodbye => return Ok(true),
        }
        Ok(false)
    }

    fn invalidate_request(&mut self, request_id: RequestId, generation: Generation) {
        let key = (request_id, generation);
        self.requests.remove(&key);
        self.pending.remove(&key);
        self.pending_contexts.remove(&key);
        self.mark_capture_cancelled(key);
        self.capture_store.cancel(request_id, generation);
        if let Some(store) = &mut self.shell_capture_store {
            store.cancel(request_id, generation);
        }
        if self.pending_completion.as_ref().is_some_and(|request| {
            request.request_id == request_id && request.generation == generation
        }) {
            self.pending_completion = None;
        }
    }

    fn finish_request(&mut self, request_id: RequestId, generation: Generation, cancelled: bool) {
        if cancelled {
            self.invalidate_request(request_id, generation);
            return;
        }

        // Native candidate generation is complete, but enrichment and
        // documentation may still publish views for this request. Retain the
        // immutable buffer/cursor context until cancellation or replacement
        // so those later views preserve ghost text and presentation semantics.
        let key = (request_id, generation);
        self.pending.remove(&key);
        self.pending_contexts.remove(&key);
    }

    fn begin_native_context(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        current_word: Option<u32>,
        expected_words: usize,
    ) -> Result<(), BridgeError> {
        let key = (request_id, generation);
        if !self.requests.contains_key(&key) {
            return Err(BridgeError::UnknownRequest {
                request_id,
                generation,
            });
        }
        if expected_words > MAX_NATIVE_CONTEXT_WORDS
            || current_word.is_some_and(|index| {
                usize::try_from(index).map_or(true, |index| index >= expected_words)
            })
        {
            return Err(BridgeError::ContextLimit);
        }
        if self.pending_contexts.contains_key(&key) {
            return Err(BridgeError::ContextAlreadyStarted {
                request_id,
                generation,
            });
        }
        self.pending_contexts.insert(
            key,
            PendingNativeContext {
                current_word,
                expected_words,
                retained_bytes: 0,
                words: Vec::with_capacity(expected_words),
            },
        );
        Ok(())
    }

    fn push_native_context(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        start: usize,
        words: Vec<RawBytes>,
    ) -> Result<(), BridgeError> {
        let context = self
            .pending_contexts
            .get_mut(&(request_id, generation))
            .ok_or(BridgeError::ContextNotStarted {
                request_id,
                generation,
            })?;
        if start != context.words.len()
            || start.saturating_add(words.len()) > context.expected_words
        {
            return Err(BridgeError::InvalidContextChunk);
        }
        let additional_bytes = words
            .iter()
            .map(RawBytes::len)
            .fold(0_usize, usize::saturating_add);
        if context.retained_bytes.saturating_add(additional_bytes) > MAX_NATIVE_CONTEXT_BYTES {
            return Err(BridgeError::ContextLimit);
        }
        context.retained_bytes += additional_bytes;
        context.words.extend(words);
        Ok(())
    }

    fn end_native_context(
        &mut self,
        request_id: RequestId,
        generation: Generation,
    ) -> Result<NativeCommandContext, BridgeError> {
        let context = self
            .pending_contexts
            .remove(&(request_id, generation))
            .ok_or(BridgeError::ContextNotStarted {
                request_id,
                generation,
            })?;
        if context.words.len() != context.expected_words {
            return Err(BridgeError::InvalidContextChunk);
        }
        Ok(NativeCommandContext {
            words: context.words,
            current_word: context.current_word,
        })
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
        client: &mut DaemonConnection,
    ) -> Result<(), ProtocolError> {
        if let Some(request) = self.pending_completion.take() {
            tracing::trace!(
                request_id = request.request_id.0,
                generation = request.generation.0,
                "dispatching debounced completion"
            );
            client.send(ClientMessage::Complete(request)).await?;
        }
        Ok(())
    }

    async fn send_completion(
        &mut self,
        request: CompletionRequest,
        client: &mut DaemonConnection,
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
        client.send(ClientMessage::Complete(request)).await
    }

    fn begin_zsh_capture(
        &mut self,
        request_id: RequestId,
        generation: Generation,
    ) -> Result<(), BridgeError> {
        if self.shell != NativeShell::Zsh {
            return Err(BridgeError::WrongShellCapture {
                capture: "Zsh",
                shell: self.shell,
            });
        }
        self.begin_capture(request_id, generation, PendingMatches::Zsh(Vec::new()))
    }

    fn begin_shell_capture(
        &mut self,
        request_id: RequestId,
        generation: Generation,
    ) -> Result<(), BridgeError> {
        if self.shell == NativeShell::Zsh {
            return Err(BridgeError::WrongShellCapture {
                capture: "generic native shell",
                shell: self.shell,
            });
        }
        self.begin_capture(request_id, generation, PendingMatches::Shell(Vec::new()))
    }

    fn begin_capture(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        matches: PendingMatches,
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
                    matches,
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

    fn push_zsh_candidate(
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
        let PendingMatches::Zsh(matches) = &mut capture.matches else {
            return Err(BridgeError::WrongShellCapture {
                capture: "Zsh candidate",
                shell: self.shell,
            });
        };
        let candidate_bytes = super::capture_size(&candidate);
        if matches.len() == self.capture_limits.max_candidates
            || capture.retained_bytes.saturating_add(candidate_bytes)
                > self.capture_limits.max_bytes
        {
            capture.dropped = capture.dropped.saturating_add(1);
        } else {
            capture.retained_bytes += candidate_bytes;
            matches.push(candidate);
        }
        Ok(())
    }

    fn push_shell_candidate(
        &mut self,
        request_id: RequestId,
        generation: Generation,
        candidate: ShellCapturedMatch,
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
        let PendingMatches::Shell(matches) = &mut capture.matches else {
            return Err(BridgeError::WrongShellCapture {
                capture: "generic native shell candidate",
                shell: self.shell,
            });
        };
        let candidate_bytes = super::shell_capture_size(&candidate);
        if matches.len() == self.capture_limits.max_candidates
            || capture.retained_bytes.saturating_add(candidate_bytes)
                > self.capture_limits.max_bytes
        {
            capture.dropped = capture.dropped.saturating_add(1);
        } else {
            capture.retained_bytes += candidate_bytes;
            matches.push(candidate);
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
        let mut outcome = match capture.matches {
            PendingMatches::Zsh(matches) => self.capture_store.install(request, matches)?,
            PendingMatches::Shell(matches) => self
                .shell_capture_store
                .as_mut()
                .ok_or(BridgeError::WrongShellCapture {
                    capture: "generic native shell",
                    shell: self.shell,
                })?
                .install(request, matches)?,
        };
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
        config.shell.shell,
        session_id,
        daemon_frame_bytes,
        config.capture_limits,
        config.viewport_rows,
        config.ghost_text,
        config.documentation,
    )
}

/// Run a bridge over arbitrary asynchronous shell streams.
///
/// The bridge creates two authenticated daemon peers: the shell client owns the
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
    let (mut client, mut worker, session_id, daemon_frame_bytes) =
        connect_bridge_peers(&config).await?;

    let input_codec = ShellWireCodec::new(config.shell_wire_limits)?;
    let output_codec = ShellWireCodec::new(config.shell_wire_limits)?;
    let mut shell_reader = FramedRead::new(shell_input, input_codec);
    let mut shell_writer = FramedWrite::new(shell_output, output_codec);
    shell_writer.set_backpressure_boundary(SHELL_OUTPUT_BACKPRESSURE_BYTES);
    let mut state = bridge_state(&config, session_id, daemon_frame_bytes)?;
    let mut debounce_timer = Box::pin(tokio::time::sleep(Duration::from_hours(24)));
    let mut debounce_armed = false;
    let mut documentation_timer = Box::pin(tokio::time::sleep(Duration::from_hours(24)));
    let mut documentation_target = None;
    let mut documentation_armed = false;

    send_startup(&mut shell_writer, session_id, &config).await?;

    loop {
        tokio::select! {
            () = &mut debounce_timer, if debounce_armed => {
                debounce_armed = false;
                state.dispatch_queued_completion(&mut client).await?;
            }
            () = &mut documentation_timer, if documentation_armed => {
                documentation_armed = false;
                if let Some(resolve) = documentation_target.clone() {
                    client.send(ClientMessage::Resolve(resolve)).await?;
                }
            }
            shell_message = shell_reader.next() => {
                let Some(shell_message) = shell_message else {
                    send_goodbye(&mut client, &mut worker).await;
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
                            .handle_shell(input, &mut client, &mut worker, &mut shell_writer)
                            .await?
                        {
                            send_goodbye(&mut client, &mut worker).await;
                            return Ok(());
                        }
                    }
                }
                synchronize_documentation_timer(
                    state.documentation_resolve_target(),
                    &mut documentation_target,
                    &mut documentation_armed,
                    documentation_timer.as_mut(),
                    config.documentation.resolve_delay,
                );
            }
            client_message = client.next() => {
                let Some(client_message) = client_message else {
                    return Err(BridgeError::DaemonClosed { role: "shell client" });
                };
                handle_client_message(
                    client_message?,
                    &mut shell_writer,
                    &mut state,
                ).await?;
                synchronize_documentation_timer(
                    state.documentation_resolve_target(),
                    &mut documentation_target,
                    &mut documentation_armed,
                    documentation_timer.as_mut(),
                    config.documentation.resolve_delay,
                );
            }
            worker_message = worker.next() => {
                let Some(worker_message) = worker_message else {
                    return Err(BridgeError::DaemonClosed { role: "completion worker" });
                };
                handle_worker_message(
                    worker_message?,
                    &mut worker,
                    &mut shell_writer,
                    &mut state,
                ).await?;
            }
        }
    }
}

async fn send_startup<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    session_id: SessionId,
    config: &BridgeConfig,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    send_shell(
        shell,
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
        shell.send(message).await?;
    }
    Ok(())
}

async fn connect_bridge_peers(
    config: &BridgeConfig,
) -> Result<(DaemonConnection, DaemonConnection, SessionId, usize), BridgeError> {
    let mut client = connect_daemon(config, PeerRole::ShellClient, None).await?;
    let client_welcome = negotiated_session(&mut client, "shell client").await?;
    let session_id = client_welcome.session_id;
    let mut worker = connect_daemon(config, PeerRole::CompletionWorker, Some(session_id)).await?;
    let worker_welcome = negotiated_session(&mut worker, "completion worker").await?;
    if worker_welcome.session_id != session_id {
        return Err(BridgeError::UnexpectedHandshake {
            role: "completion worker",
            message: "daemon attached the worker to a different session".into(),
        });
    }
    let frame_bytes = config
        .daemon_frame_bytes
        .min(client_welcome.max_frame_bytes as usize)
        .min(worker_welcome.max_frame_bytes as usize);
    Ok((client, worker, session_id, frame_bytes))
}

fn synchronize_documentation_timer(
    current: Option<ResolveRequest>,
    target: &mut Option<ResolveRequest>,
    armed: &mut bool,
    mut timer: std::pin::Pin<&mut tokio::time::Sleep>,
    delay: Duration,
) {
    if &current == target {
        return;
    }
    *target = current;
    *armed = target.is_some();
    if *armed {
        timer.as_mut().reset(tokio::time::Instant::now() + delay);
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

/// Run the bridge with a streaming input FIFO and an acknowledged output mailbox.
///
/// This transport exists for shells that cannot register a line-editor file
/// descriptor callback. The FIFO keeps shell-to-worker traffic bounded. For
/// the reverse direction, the worker writes one bounded burst, sends
/// `SIGUSR1`, and waits for the shell's `SIGUSR2` acknowledgement before
/// truncating and reusing the mailbox.
///
/// # Errors
///
/// Rejects unsafe paths and propagates signal, I/O, and bridge failures.
pub async fn run_signal_bridge(
    config: BridgeConfig,
    input_path: PathBuf,
    output_path: PathBuf,
    shell_process_id: u32,
) -> Result<(), BridgeError> {
    validate_fifo(&input_path)?;
    validate_mailbox(&output_path)?;
    let (input, input_guard) = open_guarded_fifo(input_path).await?;
    let output = tokio::fs::OpenOptions::new()
        .write(true)
        .open(&output_path)
        .await?;

    let (bridge_output, output_receiver) = tokio::io::duplex(256 * 1024);
    let output_task = tokio::spawn(pump_mailbox_output(
        output,
        output_receiver,
        shell_process_id,
    ));
    let result = run_bridge(config, input, bridge_output).await;
    drop(input_guard);
    output_task.abort();
    let _ = output_task.await;
    result
}

async fn open_guarded_fifo(path: PathBuf) -> Result<(tokio::fs::File, std::fs::File), BridgeError> {
    tokio::task::spawn_blocking(move || {
        let input = OpenOptions::new()
            .read(true)
            .custom_flags(nix::libc::O_NONBLOCK)
            .open(&path)?;
        let guard = OpenOptions::new()
            .write(true)
            .custom_flags(nix::libc::O_NONBLOCK)
            .open(&path)?;
        let flags =
            nix::fcntl::fcntl(&input, nix::fcntl::FcntlArg::F_GETFL).map_err(io::Error::from)?;
        let mut flags = nix::fcntl::OFlag::from_bits_retain(flags);
        flags.remove(nix::fcntl::OFlag::O_NONBLOCK);
        nix::fcntl::fcntl(&input, nix::fcntl::FcntlArg::F_SETFL(flags)).map_err(io::Error::from)?;
        Ok((tokio::fs::File::from_std(input), guard))
    })
    .await?
}

async fn pump_mailbox_output(
    mut output: tokio::fs::File,
    mut bridge: tokio::io::DuplexStream,
    shell_process_id: u32,
) -> Result<(), BridgeError> {
    let shell_process_id = i32::try_from(shell_process_id)
        .map_err(|_| BridgeError::InvalidShellProcessId(shell_process_id))?;
    let shell_process = nix::unistd::Pid::from_raw(shell_process_id);
    let mut acknowledgement =
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::user_defined2())?;
    let mut read_buffer = vec![0_u8; 64 * 1024];
    let mut framed_bytes = BytesMut::new();
    let mut codec = ShellWireCodec::default();
    loop {
        let count = bridge.read(&mut read_buffer).await?;
        if count == 0 {
            return Ok(());
        }
        framed_bytes.extend_from_slice(&read_buffer[..count]);
        let mut wrote_record = false;
        while let Some(message) = codec.decode(&mut framed_bytes)? {
            output.write_all(&mailbox_record(&message)).await?;
            wrote_record = true;
        }
        if !wrote_record {
            continue;
        }
        output.flush().await?;
        nix::sys::signal::kill(shell_process, nix::sys::signal::Signal::SIGUSR1)
            .map_err(BridgeError::Signal)?;
        if acknowledgement.recv().await.is_none() {
            return Ok(());
        }
        output.set_len(0).await?;
        output.seek(SeekFrom::Start(0)).await?;
    }
}

fn mailbox_record(message: &ShellWireMessage) -> Vec<u8> {
    let mut record = Vec::new();
    record.extend_from_slice(message.command.as_bytes());
    record.push(b'\t');
    record.extend_from_slice(message.fields.len().to_string().as_bytes());
    for field in &message.fields {
        record.push(b'\t');
        for byte in field.as_slice() {
            const HEX: &[u8; 16] = b"0123456789abcdef";
            record.extend_from_slice(b"\\x");
            record.push(HEX[usize::from(*byte >> 4)]);
            record.push(HEX[usize::from(*byte & 0x0f)]);
        }
    }
    record.push(b'\n');
    record
}

fn validate_mailbox(path: &PathBuf) -> Result<(), BridgeError> {
    let metadata = fs::symlink_metadata(path)?;
    if !metadata.file_type().is_file() || metadata.file_type().is_symlink() {
        return Err(BridgeError::InvalidMailbox { path: path.clone() });
    }
    if metadata.uid() != nix::unistd::getuid().as_raw()
        || metadata.permissions().mode() & 0o077 != 0
    {
        return Err(BridgeError::InsecureMailbox { path: path.clone() });
    }
    Ok(())
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
            process_id: if role == PeerRole::ShellClient {
                config.shell_process_id
            } else {
                std::process::id()
            },
            shell: Some(config.shell.clone()),
            attach_session,
            attach_process_id: None,
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

async fn handle_client_message<W>(
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
                send_candidate_view(
                    shell,
                    window,
                    &state.capture_store,
                    state.shell_capture_store.as_ref(),
                    state.documentation,
                )
                .await?;
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
            state.invalidate_request(request_id, generation);
            send_request_event(shell, "request-cancelled", request_id, generation, []).await?;
        }
        ServerMessage::RequestFinished {
            request_id,
            generation,
            cancelled,
        } => {
            state.finish_request(request_id, generation, cancelled);
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
            forward_documentation(shell, state, request_id, generation, item_id, documentation)
                .await?;
        }
        ServerMessage::PresentationChanged { external } => {
            send_shell(shell, "presentation", [bool_field(external)]).await?;
        }
        ServerMessage::Pong { nonce } => {
            send_shell(shell, "pong", [nonce.to_string().into()]).await?;
        }
        ServerMessage::Error {
            code,
            message,
            request_id,
        } => {
            send_daemon_error(shell, code, message, request_id).await?;
        }
        ServerMessage::SelectionFinished(_)
        | ServerMessage::Welcome(_)
        | ServerMessage::CompletionRequested(_)
        | ServerMessage::NativeContextPublished(_)
        | ServerMessage::SelectionRequested(_)
        | ServerMessage::ResolveRequested(_)
        | ServerMessage::Candidates(_) => {}
    }
    Ok(())
}

async fn forward_documentation<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    state: &mut BridgeState,
    request_id: RequestId,
    generation: Generation,
    item_id: ItemId,
    documentation: sense_model::DocumentationState,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let selected = state.install_documentation(request_id, generation, &item_id, documentation);
    if selected && state.documentation.enabled() {
        let Some(window) = state.current_window() else {
            return Ok(());
        };
        let presentation = view_presentation(&window, state.documentation);
        send_view_presentation(
            shell,
            request_id,
            generation,
            window.view.revision,
            &presentation,
        )
        .await?;
        shell.flush().await?;
    }
    Ok(())
}

async fn handle_worker_message<W>(
    message: ServerMessage,
    worker: &mut DaemonConnection,
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
                shell = ?state.shell,
                "requesting native completion capture"
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
            state.invalidate_request(request_id, generation);
        }
        ServerMessage::SelectionRequested(selection) => {
            let rejection = selection.clone();
            let result = if state.shell == NativeShell::Zsh {
                state
                    .capture_store
                    .acceptance_by_item(
                        selection.request_id,
                        selection.generation,
                        &selection.item_id,
                    )
                    .map(|route| Acceptance::Zsh(Box::new(route.clone())))
            } else {
                state
                    .shell_capture_store
                    .as_ref()
                    .ok_or(CaptureError::InvalidShellStore)
                    .and_then(|store| {
                        store.acceptance_by_item(
                            selection.request_id,
                            selection.generation,
                            &selection.item_id,
                        )
                    })
                    .map(|route| Acceptance::Shell(route.clone()))
            };
            match result {
                Ok(route) => send_acceptance(shell, selection.item_id, route).await?,
                Err(error) => {
                    worker
                        .send(ClientMessage::ReportSelection(
                            sense_protocol::SelectionResult {
                                selection: rejection,
                                applied: false,
                            },
                        ))
                        .await?;
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
            send_daemon_error(shell, code, message, request_id).await?;
        }
        ServerMessage::Welcome(_)
        | ServerMessage::CandidateView(_)
        | ServerMessage::NativeContextPublished(_)
        | ServerMessage::ResolveRequested(_)
        | ServerMessage::RequestStarted { .. }
        | ServerMessage::Candidates(_)
        | ServerMessage::RequestFinished { .. }
        | ServerMessage::Documentation { .. }
        | ServerMessage::PresentationChanged { .. }
        | ServerMessage::SelectionFinished(_)
        | ServerMessage::Pong { .. } => {}
    }
    Ok(())
}

async fn send_daemon_error<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    code: String,
    message: String,
    request_id: Option<RequestId>,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    send_shell(
        shell,
        "error",
        [
            code.into(),
            message.into(),
            request_id.map_or_else(RawBytes::default, |id| id.0.to_string().into()),
        ],
    )
    .await
}

fn shell_input_name(input: &ShellInput) -> &'static str {
    match input {
        ShellInput::Complete(_) => "complete",
        ShellInput::Cancel(_, _) => "cancel",
        ShellInput::Select(_, _, _) => "select",
        ShellInput::SelectionFinished(_, _, _, _) => "selection-finished",
        ShellInput::Navigate(_, _, _) => "navigate",
        ShellInput::ZshCaptureBegin(_, _) => "zsh-capture-begin",
        ShellInput::ZshCandidate(_, _, _) => "zsh-candidate",
        ShellInput::ZshCandidateChunk(_, _, _) => "zsh-command-candidates",
        ShellInput::ShellCaptureBegin(_, _) => "shell-capture-begin",
        ShellInput::ShellCandidate(_, _, _) => "shell-candidate",
        ShellInput::ContextBegin(_, _, _, _) => "context-begin",
        ShellInput::ContextChunk(_, _, _, _) => "context-chunk",
        ShellInput::ContextEnd(_, _) => "context-end",
        ShellInput::CaptureEnd(_, _) => "capture-end",
        ShellInput::Ping(_) => "ping",
        ShellInput::Goodbye => "goodbye",
    }
}

async fn send_candidate_view<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    window: WindowedView,
    capture_store: &CaptureStore,
    shell_capture_store: Option<&ShellCaptureStore>,
    documentation: DocumentationPolicy,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let presentation = view_presentation(&window, documentation);
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
        window.max_label_cells.to_string().into(),
        window.max_described_cells.to_string().into(),
        view.sources_pending.len().to_string().into(),
    ];
    begin_fields.extend(
        view.sources_pending
            .iter()
            .map(|source| RawBytes::from(source.0.as_str())),
    );
    feed_shell(shell, "view-begin", begin_fields).await?;
    for (items, ghost_texts) in view
        .items
        .chunks(VIEW_CHUNK_ITEMS)
        .zip(window.ghost_texts.chunks(VIEW_CHUNK_ITEMS))
    {
        feed_view_chunk(
            shell,
            view.request_id,
            view.generation,
            items,
            ghost_texts,
            capture_store,
            shell_capture_store,
        )
        .await?;
    }
    send_view_presentation(
        shell,
        view.request_id,
        view.generation,
        view.revision,
        &presentation,
    )
    .await?;
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

#[derive(Debug)]
struct ViewPresentation {
    menu_width: u16,
    documentation: Option<(ItemId, DocumentationPanel)>,
}

fn view_presentation(window: &WindowedView, policy: DocumentationPolicy) -> ViewPresentation {
    let selected = window
        .view
        .selected_index
        .and_then(|index| window.view.items.get(index as usize));
    let documentation = policy
        .enabled()
        .then_some(selected)
        .flatten()
        .and_then(|item| {
            let sense_model::DocumentationState::Resolved(content) = &item.documentation else {
                return None;
            };
            Some(content)
        });
    let candidate_cells = if policy.menu.descriptions {
        window.max_described_cells
    } else {
        window.max_label_cells
    };
    let scrollbar_cells =
        usize::from(policy.menu.scrollbar && window.total > usize::from(policy.menu.menu_max_rows));
    let preferred_menu_width = u16::try_from(
        candidate_cells
            .saturating_add(usize::from(policy.menu.menu_chrome_cells))
            .saturating_add(scrollbar_cells),
    )
    .unwrap_or(u16::MAX)
    .clamp(policy.menu.menu_min_width, policy.menu.menu_max_width);
    let layout = sense_present::layout(
        documentation,
        PresentationRequest {
            terminal_columns: window.terminal.columns,
            preferred_menu_width,
            minimum_menu_width: policy.menu.menu_min_width,
            preference: policy.layout.placement,
            side_min_columns: policy.layout.side_min_columns,
            documentation_width_ratio: policy.layout.width_ratio,
            documentation_max_rows: policy.layout.max_rows,
            documentation_padding: policy.layout.padding,
            bordered: policy.layout.bordered,
            render_markdown: policy.layout.render_markdown,
        },
    );
    ViewPresentation {
        menu_width: layout.menu_width,
        documentation: selected
            .map(|item| item.id.clone())
            .zip(layout.documentation),
    }
}

async fn send_view_presentation<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    request_id: RequestId,
    generation: Generation,
    revision: u64,
    presentation: &ViewPresentation,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    feed_shell(
        shell,
        "view-layout",
        [
            request_id.0.to_string().into(),
            generation.0.to_string().into(),
            revision.to_string().into(),
            presentation.menu_width.to_string().into(),
        ],
    )
    .await?;
    let Some((item_id, panel)) = &presentation.documentation else {
        return feed_shell(
            shell,
            "documentation-clear",
            [
                request_id.0.to_string().into(),
                generation.0.to_string().into(),
            ],
        )
        .await;
    };
    feed_shell(
        shell,
        "documentation-begin",
        [
            request_id.0.to_string().into(),
            generation.0.to_string().into(),
            item_id.0.as_str().into(),
            documentation_placement_name(panel.placement).into(),
            panel.width.to_string().into(),
            panel.lines.len().to_string().into(),
            bool_field(panel.truncated),
        ],
    )
    .await?;
    for lines in panel.lines.chunks(32) {
        send_documentation_chunk(shell, request_id, generation, item_id, lines).await?;
    }
    feed_shell(
        shell,
        "documentation-end",
        [
            request_id.0.to_string().into(),
            generation.0.to_string().into(),
            item_id.0.as_str().into(),
        ],
    )
    .await
}

async fn send_documentation_chunk<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    request_id: RequestId,
    generation: Generation,
    item_id: &ItemId,
    lines: &[DocumentationLine],
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let mut fields = Vec::with_capacity(4 + lines.len() * 3);
    fields.extend([
        request_id.0.to_string().into(),
        generation.0.to_string().into(),
        item_id.0.as_str().into(),
        lines.len().to_string().into(),
    ]);
    for line in lines {
        fields.extend([
            documentation_line_kind_name(line.kind).into(),
            line.cells.to_string().into(),
            line.text.as_str().into(),
        ]);
    }
    feed_shell(shell, "documentation-chunk", fields).await
}

const fn documentation_placement_name(placement: DocumentationPlacement) -> &'static str {
    match placement {
        DocumentationPlacement::Side => "side",
        DocumentationPlacement::Below => "below",
    }
}

const fn documentation_line_kind_name(kind: sense_present::DocumentationLineKind) -> &'static str {
    match kind {
        sense_present::DocumentationLineKind::Text => "text",
        sense_present::DocumentationLineKind::Heading => "heading",
        sense_present::DocumentationLineKind::Code => "code",
        sense_present::DocumentationLineKind::ListItem => "list-item",
        sense_present::DocumentationLineKind::Quote => "quote",
        sense_present::DocumentationLineKind::Separator => "separator",
    }
}

async fn feed_view_chunk<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    request_id: RequestId,
    generation: Generation,
    items: &[CompletionItem],
    ghost_texts: &[Option<GhostText>],
    capture_store: &CaptureStore,
    shell_capture_store: Option<&ShellCaptureStore>,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    debug_assert_eq!(items.len(), ghost_texts.len());
    let mut fields = Vec::with_capacity(3 + items.len() * VIEW_CHUNK_ITEM_FIELDS);
    fields.extend([
        request_id.0.to_string().into(),
        generation.0.to_string().into(),
        items.len().to_string().into(),
    ]);
    for (item, ghost_text) in items.iter().zip(ghost_texts) {
        let acceptance = acceptance_summary(
            request_id,
            generation,
            &item.id,
            capture_store,
            shell_capture_store,
        );
        fields.extend([
            item.id.0.as_str().into(),
            item.label.as_str().into(),
            UnicodeWidthStr::width(item.label.as_str())
                .to_string()
                .into(),
            completion_kind_name(item.kind).into(),
            optional_text(item.detail.as_deref()),
            item.detail
                .as_deref()
                .map_or(0, UnicodeWidthStr::width)
                .to_string()
                .into(),
            item.group
                .as_ref()
                .map_or_else(RawBytes::default, |group| group.0.as_str().into()),
            acceptance
                .as_ref()
                .map_or_else(RawBytes::default, |(source, _)| source.as_str().into()),
            acceptance.map_or_else(RawBytes::default, |(_, identity)| identity),
            label_match_ranges(item).into(),
            ghost_text
                .as_ref()
                .map_or_else(RawBytes::default, |ghost| ghost.edit.new_text.clone()),
        ]);
    }
    feed_shell(shell, "view-chunk", fields).await
}

fn acceptance_summary(
    request_id: RequestId,
    generation: Generation,
    item_id: &ItemId,
    capture_store: &CaptureStore,
    shell_capture_store: Option<&ShellCaptureStore>,
) -> Option<(String, RawBytes)> {
    if let Ok(route) = capture_store.acceptance_by_item(request_id, generation, item_id) {
        return Some(("zsh".into(), route.acceptance_identity.clone()));
    }
    let route = shell_capture_store?
        .acceptance_by_item(request_id, generation, item_id)
        .ok()?;
    Some((
        route.shell.source_name().into(),
        route.acceptance_identity.clone(),
    ))
}

fn completion_ghost_text(
    request: &CompletionRequest,
    item: &CompletionItem,
    policy: GhostTextPolicy,
    is_unique_prefix: bool,
) -> Option<GhostText> {
    // ZLE's display-only POSTDISPLAY begins after the editable buffer. It can
    // represent an inline suffix without mutating BUFFER only at end-of-line.
    if !policy.enabled || !is_unique_prefix || request.cursor.as_usize() != request.buffer.len() {
        return None;
    }
    let confidence = completion_confidence(item.confidence);
    if confidence < policy.minimum_confidence {
        return None;
    }
    let matched = item.match_result.as_ref()?;
    if !matched.prefix || matched.indices.is_empty() {
        return None;
    }
    let filter_text = item.filter_text.as_deref().unwrap_or(&item.label);
    let insertion = std::str::from_utf8(item.edit.new_text.as_slice()).ok()?;
    // A presentation label can differ from what Zsh inserts. Deriving a
    // suffix across that boundary would display text that acceptance does not
    // produce, so only identical filter/insertion text is eligible.
    if filter_text != insertion {
        return None;
    }
    let prefix_end = contiguous_prefix_end(filter_text, &matched.indices)?;
    let suffix = insertion.get(prefix_end..)?;
    if suffix.is_empty() || suffix.chars().any(char::is_control) {
        return None;
    }
    Some(GhostText {
        edit: TextEdit::new(
            TextRange::new(request.cursor.0, request.cursor.0),
            RawBytes::from(suffix),
        ),
        source: item.source.clone(),
        confidence,
    })
}

fn contiguous_prefix_end(text: &str, indices: &[u32]) -> Option<usize> {
    let starts: Vec<_> = text.char_indices().map(|(offset, _)| offset).collect();
    if indices.len() > starts.len() {
        return None;
    }
    for (expected, actual) in starts.iter().zip(indices) {
        if u32::try_from(*expected).ok()? != *actual {
            return None;
        }
    }
    starts.get(indices.len()).copied().or(Some(text.len()))
}

const fn completion_confidence(confidence: Confidence) -> f32 {
    match confidence {
        Confidence::Advisory => 0.25,
        Confidence::Inferred => 0.5,
        Confidence::Partial => 0.75,
        Confidence::Authoritative => 1.0,
    }
}

fn label_match_ranges(item: &CompletionItem) -> String {
    let Some(result) = &item.match_result else {
        return String::new();
    };
    let filter_text = item.filter_text.as_deref().unwrap_or(&item.label);
    if filter_text != item.label || result.indices.is_empty() {
        return String::new();
    }

    let mut ranges = Vec::<(usize, usize)>::new();
    let mut characters = item.label.char_indices().peekable();
    let mut character_index = 0;
    while let Some((byte_start, _)) = characters.next() {
        let byte_end = characters
            .peek()
            .map_or(item.label.len(), |(next_start, _)| *next_start);
        let matched = result
            .indices
            .iter()
            .any(|index| (*index as usize) >= byte_start && (*index as usize) < byte_end);
        if matched {
            match ranges.last_mut() {
                Some((_, end)) if *end == character_index => *end += 1,
                _ => ranges.push((character_index, character_index + 1)),
            }
        }
        character_index += 1;
    }

    ranges
        .into_iter()
        .map(|(start, end)| format!("{start}:{end}"))
        .collect::<Vec<_>>()
        .join(",")
}

async fn send_acceptance<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    item_id: ItemId,
    route: Acceptance,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    let route = match route {
        Acceptance::Zsh(route) => *route,
        Acceptance::Shell(route) => return send_shell_acceptance(shell, item_id, route).await,
    };
    let metadata = route.insertion_metadata;
    let mut fields = vec![
        route.request_id.0.to_string().into(),
        route.generation.0.to_string().into(),
        item_id.0.into(),
        route.ordinal.to_string().into(),
        route.acceptance_identity,
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

async fn send_shell_acceptance<W>(
    shell: &mut FramedWrite<W, ShellWireCodec>,
    item_id: ItemId,
    route: ShellAcceptanceRoute,
) -> Result<(), ShellWireError>
where
    W: AsyncWrite + Unpin,
{
    send_shell(
        shell,
        match route.shell {
            NativeShell::Fish => "accept-fish",
            NativeShell::Bash => "accept-bash",
            NativeShell::Zsh => unreachable!("Zsh uses its metadata-complete route"),
        },
        [
            route.request_id.0.to_string().into(),
            route.generation.0.to_string().into(),
            item_id.0.into(),
            route.insertion,
            route.replace_range.start.0.to_string().into(),
            route.replace_range.end.0.to_string().into(),
            bool_field(route.append_space),
            route.acceptance_identity,
        ],
    )
    .await
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
        "selection-finished" => parse_selection_finished(&message.fields).map_err(invalid),
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
        "zsh-capture-begin" => {
            expect_fields(&message.fields, 2).map_err(invalid)?;
            Ok(ShellInput::ZshCaptureBegin(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
            ))
        }
        "zsh-candidate" => parse_candidate(&message.fields)
            .map(|(request_id, generation, candidate)| {
                ShellInput::ZshCandidate(request_id, generation, Box::new(candidate))
            })
            .map_err(invalid),
        "zsh-command-candidates" => parse_command_candidates(&message.fields)
            .map(|(request_id, generation, candidates)| {
                ShellInput::ZshCandidateChunk(request_id, generation, candidates)
            })
            .map_err(invalid),
        "shell-capture-begin" => {
            expect_fields(&message.fields, 2).map_err(invalid)?;
            Ok(ShellInput::ShellCaptureBegin(
                parse_request_id(&message.fields[0]).map_err(invalid)?,
                parse_generation(&message.fields[1]).map_err(invalid)?,
            ))
        }
        "shell-candidate" => parse_shell_candidate(&message.fields)
            .map(|(request_id, generation, candidate)| {
                ShellInput::ShellCandidate(request_id, generation, Box::new(candidate))
            })
            .map_err(invalid),
        "context-begin" | "context-chunk" | "context-end" => {
            parse_context_message(message).map_err(invalid)
        }
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

fn parse_context_message(message: &ShellWireMessage) -> Result<ShellInput, String> {
    match message.command.as_str() {
        "context-begin" => {
            expect_fields(&message.fields, 4)?;
            let current_word = if message.fields[2].is_empty() {
                None
            } else {
                Some(parse_u32(&message.fields[2], "current word")?)
            };
            Ok(ShellInput::ContextBegin(
                parse_request_id(&message.fields[0])?,
                parse_generation(&message.fields[1])?,
                current_word,
                parse_usize(&message.fields[3], "context word count")?,
            ))
        }
        "context-chunk" => {
            if message.fields.len() < 4 {
                return Err("context-chunk requires at least 4 fields".into());
            }
            let count = parse_usize(&message.fields[3], "context chunk count")?;
            expect_fields(&message.fields, 4_usize.saturating_add(count))?;
            Ok(ShellInput::ContextChunk(
                parse_request_id(&message.fields[0])?,
                parse_generation(&message.fields[1])?,
                parse_usize(&message.fields[2], "context chunk start")?,
                message.fields[4..].to_vec(),
            ))
        }
        "context-end" => {
            expect_fields(&message.fields, 2)?;
            Ok(ShellInput::ContextEnd(
                parse_request_id(&message.fields[0])?,
                parse_generation(&message.fields[1])?,
            ))
        }
        _ => unreachable!("caller accepts only native context commands"),
    }
}

fn parse_shell_candidate(
    fields: &[RawBytes],
) -> Result<(RequestId, Generation, ShellCapturedMatch), String> {
    expect_fields(fields, 14)?;
    let insertion = fields[2].clone();
    let label = if fields[3].is_empty() {
        insertion.display_lossy()
    } else {
        parse_utf8(&fields[3], "candidate label")?
    };
    Ok((
        parse_request_id(&fields[0])?,
        parse_generation(&fields[1])?,
        ShellCapturedMatch {
            insertion,
            label,
            description: optional_lossy(&fields[4]),
            group: optional_lossy(&fields[5]),
            replace_range: TextRange::new(
                parse_u32(&fields[6], "replacement start")?,
                parse_u32(&fields[7], "replacement end")?,
            ),
            kind: parse_completion_kind(&fields[8])?,
            original_order: parse_u32(&fields[9], "original order")?,
            append_space: parse_boolean(&fields[10], "append-space")?,
            partial_accept: parse_boolean(&fields[11], "partial-accept")?,
            acceptance_identity: fields[12].clone(),
            resource_path: optional_bytes(&fields[13]),
        },
    ))
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
    if fields.len() < 28 {
        return Err("candidate requires at least 28 fields".into());
    }
    let matcher_count = parse_usize(&fields[27], "matcher count")?;
    let expected = 28_usize
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
            resource_path: optional_bytes(&fields[26]),
            flags,
            acceptance_identity: fields[13].clone(),
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
                matcher_specs: fields[28..].to_vec(),
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
            resource_path: None,
            flags: ZshMatchFlags::empty(),
            insertion_metadata: insertion_metadata.clone(),
            acceptance_identity: ordinal.to_string().into(),
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

async fn send_goodbye(client: &mut DaemonConnection, worker: &mut DaemonConnection) {
    let _ = client.send(ClientMessage::Goodbye).await;
    let _ = worker.send(ClientMessage::Goodbye).await;
}

fn parse_request_id(field: &RawBytes) -> Result<RequestId, String> {
    parse_u64(field, "request identifier").map(RequestId)
}

fn parse_selection_finished(fields: &[RawBytes]) -> Result<ShellInput, String> {
    expect_fields(fields, 4)?;
    Ok(ShellInput::SelectionFinished(
        parse_request_id(&fields[0])?,
        parse_generation(&fields[1])?,
        ItemId(parse_utf8(&fields[2], "item identifier")?),
        parse_boolean(&fields[3], "selection result")?,
    ))
}

fn selection_result_message(
    session_id: SessionId,
    request_id: RequestId,
    generation: Generation,
    item_id: ItemId,
    applied: bool,
) -> ClientMessage {
    ClientMessage::ReportSelection(sense_protocol::SelectionResult {
        selection: sense_protocol::SelectionRequest {
            session_id,
            request_id,
            generation,
            item_id,
        },
        applied,
    })
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

fn parse_boolean(field: &RawBytes, name: &str) -> Result<bool, String> {
    match field.as_slice() {
        b"0" => Ok(false),
        b"1" => Ok(true),
        _ => Err(format!("{name} must be 0 or 1")),
    }
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
    }
}

fn optional_lossy(field: &RawBytes) -> Option<String> {
    (!field.is_empty()).then(|| field.display_lossy())
}

fn optional_bytes(field: &RawBytes) -> Option<RawBytes> {
    (!field.is_empty()).then(|| field.clone())
}

fn optional_text(value: Option<&str>) -> RawBytes {
    value.map_or_else(RawBytes::default, Into::into)
}

fn bool_field(value: bool) -> RawBytes {
    RawBytes::from(if value { "1" } else { "0" })
}

fn server_message_name(message: &ServerMessage) -> &'static str {
    match message {
        ServerMessage::Welcome(_) => "welcome",
        ServerMessage::CompletionRequested(_) => "completion-requested",
        ServerMessage::NativeContextPublished(_) => "native-context-published",
        ServerMessage::RequestCancelled { .. } => "request-cancelled",
        ServerMessage::SelectionRequested(_) => "selection-requested",
        ServerMessage::ResolveRequested(_) => "resolve-requested",
        ServerMessage::RequestStarted { .. } => "request-started",
        ServerMessage::Candidates(_) => "candidates",
        ServerMessage::CandidateView(_) => "candidate-view",
        ServerMessage::RequestFinished { .. } => "request-finished",
        ServerMessage::Documentation { .. } => "documentation",
        ServerMessage::PresentationChanged { .. } => "presentation-changed",
        ServerMessage::SelectionFinished(_) => "selection-finished",
        ServerMessage::Pong { .. } => "pong",
        ServerMessage::Error { .. } => "error",
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use sense_model::{MatchResult, SourceId, TextEdit};

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
                    CompletionItem::native(
                        format!("item-{index}"),
                        NativeShell::Zsh,
                        label.clone(),
                        TextEdit::new(TextRange::new(0, 0), label),
                        format!("item-{index}"),
                    )
                })
                .collect(),
            is_final: true,
            is_incomplete: true,
        }
    }

    fn request(buffer: &str) -> CompletionRequest {
        CompletionRequest {
            session_id: SessionId::new(),
            request_id: RequestId(1),
            generation: Generation(2),
            context_epoch: ContextEpoch::default(),
            buffer: RawBytes::from(buffer),
            cursor: ByteOffset(u32::try_from(buffer.len()).unwrap()),
            cwd: RawBytes::from("/tmp"),
            keymap: "emacs".into(),
            terminal: TerminalDimensions::default(),
            trigger: TriggerKind::Automatic,
            environment: BTreeMap::new(),
        }
    }

    #[test]
    fn ghost_text_is_only_the_unique_prefix_suffix() {
        let request = request("systemctl res");
        let mut item = CompletionItem::native(
            "restart",
            NativeShell::Zsh,
            "restart",
            TextEdit::new(TextRange::new(10, 13), "restart"),
            "restart",
        );
        item.filter_text = Some("restart".into());
        item.match_result = Some(MatchResult {
            score: 1,
            indices: vec![0, 1, 2],
            exact: false,
            prefix: true,
        });

        let ghost =
            completion_ghost_text(&request, &item, GhostTextPolicy::default(), true).unwrap();
        assert_eq!(ghost.edit.range, TextRange::new(13, 13));
        assert_eq!(ghost.edit.new_text.as_slice(), b"tart");
        assert_eq!(ghost.source, SourceId("zsh".into()));
        assert!((ghost.confidence - 1.0).abs() < f32::EPSILON);

        item.match_result.as_mut().unwrap().prefix = false;
        assert!(completion_ghost_text(&request, &item, GhostTextPolicy::default(), true).is_none());
        item.match_result.as_mut().unwrap().prefix = true;
        assert!(
            completion_ghost_text(&request, &item, GhostTextPolicy::default(), false).is_none()
        );
    }

    #[test]
    fn ghost_text_rejects_non_contiguous_and_presentation_only_matches() {
        let request = request("command ab");
        let mut item = CompletionItem::native(
            "candidate",
            NativeShell::Zsh,
            "alpha-beta",
            TextEdit::new(TextRange::new(8, 10), "alpha-beta"),
            "candidate",
        );
        item.filter_text = Some("alpha-beta".into());
        item.match_result = Some(MatchResult {
            score: 1,
            indices: vec![0, 6],
            exact: false,
            prefix: true,
        });
        assert!(completion_ghost_text(&request, &item, GhostTextPolicy::default(), true).is_none());

        item.match_result.as_mut().unwrap().indices = vec![0, 1];
        item.filter_text = Some("display-only".into());
        assert!(completion_ghost_text(&request, &item, GhostTextPolicy::default(), true).is_none());
    }

    #[test]
    fn finished_native_generation_retains_context_for_later_views() {
        let mut state = BridgeState::new(
            NativeShell::Zsh,
            SessionId::new(),
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            10,
            GhostTextPolicy::default(),
            DocumentationPolicy::default(),
        )
        .unwrap();
        let request = request("systemctl res");
        let key = (request.request_id, request.generation);
        state.highest_generation = request.generation.0;
        state.requests.insert(key, request);
        state.finish_request(key.0, key.1, false);
        assert!(state.requests.contains_key(&key));

        let mut item = CompletionItem::native(
            "restart",
            NativeShell::Zsh,
            "restart",
            TextEdit::new(TextRange::new(10, 13), "restart"),
            "restart",
        );
        item.filter_text = Some("restart".into());
        item.match_result = Some(MatchResult {
            score: 1,
            indices: vec![0, 1, 2],
            exact: false,
            prefix: true,
        });
        state.install_view(CandidateView {
            session_id: state.session_id,
            request_id: key.0,
            generation: key.1,
            revision: 2,
            items: vec![item],
            selected_index: Some(0),
            matched_before_limit: 1,
            sources_pending: Vec::new(),
            is_final: true,
            is_incomplete: false,
            is_settled: true,
        });

        let window = state.current_window().unwrap();
        let ghost = window.ghost_texts[0].as_ref().unwrap();
        assert_eq!(ghost.edit.new_text.as_slice(), b"tart");

        state.finish_request(key.0, key.1, true);
        assert!(!state.requests.contains_key(&key));
    }

    #[test]
    fn label_match_ranges_convert_frizbee_byte_offsets_to_character_ranges() {
        let mut item = CompletionItem::native(
            "unicode",
            NativeShell::Zsh,
            "aé😀z",
            TextEdit::new(TextRange::new(0, 0), "aé😀z"),
            "unicode",
        );
        item.match_result = Some(MatchResult {
            score: 1,
            indices: vec![1, 2, 7],
            exact: false,
            prefix: false,
        });

        assert_eq!(label_match_ranges(&item), "1:2,3:4");
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
    fn mailbox_records_are_line_safe_and_byte_exact() {
        let record = mailbox_record(&ShellWireMessage::new(
            "view-chunk",
            vec![
                RawBytes::from("λ\n"),
                RawBytes::default(),
                RawBytes::from(&b"\xff"[..]),
            ],
        ));
        assert_eq!(record, b"view-chunk\t3\t\\xce\\xbb\\x0a\t\t\\xff\n");
    }

    #[test]
    fn delayed_capture_is_ignored_after_its_cancellation_tombstone_expires() {
        let mut state = BridgeState::new(
            NativeShell::Zsh,
            SessionId::new(),
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            10,
            GhostTextPolicy::default(),
            DocumentationPolicy::default(),
        )
        .unwrap();
        state.highest_generation = 400;

        let delayed_key = (RequestId(45), Generation(45));
        assert!(!state.cancelled_captures.contains(&delayed_key));
        state
            .begin_zsh_capture(delayed_key.0, delayed_key.1)
            .unwrap();
        assert!(
            state
                .end_capture(delayed_key.0, delayed_key.1)
                .unwrap()
                .is_empty()
        );

        let future = (RequestId(401), Generation(401));
        assert!(matches!(
            state.begin_zsh_capture(future.0, future.1),
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
        assert_eq!(candidate.acceptance_identity.as_slice(), b"4");
        assert_eq!(candidate.original_order, 3);
    }

    #[test]
    fn popup_width_metadata_is_unicode_aware_and_stable_across_navigation() {
        let mut state = BridgeState::new(
            NativeShell::Zsh,
            SessionId::new(),
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            2,
            GhostTextPolicy::default(),
            DocumentationPolicy::default(),
        )
        .unwrap();
        let source = batch(3);
        let mut items = source.items;
        items[0].label = "short".into();
        items[0].detail = Some("plain".into());
        items[1].label = "界界界".into();
        items[1].detail = Some("documentation".into());
        items[2].label = "mid".into();
        let view = CandidateView {
            session_id: source.session_id,
            request_id: source.request_id,
            generation: source.generation,
            revision: 1,
            items,
            selected_index: Some(0),
            matched_before_limit: 3,
            sources_pending: Vec::new(),
            is_final: true,
            is_incomplete: false,
            is_settled: true,
        };
        state.install_view(view);
        let first = state.current_window().unwrap();
        assert_eq!(first.max_label_cells, 6);
        assert_eq!(first.max_described_cells, 21);

        state.navigate(RequestId(1), Generation(2), Navigation::PageDown);
        let navigated = state.current_window().unwrap();
        assert_eq!(navigated.max_label_cells, first.max_label_cells);
        assert_eq!(navigated.max_described_cells, first.max_described_cells);
    }

    #[test]
    fn documentation_resolution_tracks_only_the_current_selected_item() {
        let session_id = SessionId::new();
        let mut state = BridgeState::new(
            NativeShell::Zsh,
            session_id,
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            2,
            GhostTextPolicy::default(),
            DocumentationPolicy::default(),
        )
        .unwrap();
        state.highest_generation = 2;
        let mut items = batch(2).items;
        items[0].documentation = sense_model::DocumentationState::Unresolved;
        state.install_view(CandidateView {
            session_id,
            request_id: RequestId(1),
            generation: Generation(2),
            revision: 1,
            items,
            selected_index: Some(0),
            matched_before_limit: 2,
            sources_pending: Vec::new(),
            is_final: true,
            is_incomplete: false,
            is_settled: true,
        });

        let target = state.documentation_resolve_target().unwrap();
        assert_eq!(target.item_id, ItemId("item-0".into()));
        assert!(state.install_documentation(
            RequestId(1),
            Generation(2),
            &target.item_id,
            sense_model::DocumentationState::Resolved(sense_model::MarkupContent {
                kind: sense_model::MarkupKind::PlainText,
                value: "Native documentation".into(),
            }),
        ));
        assert!(state.documentation_resolve_target().is_none());

        assert!(state.install_documentation(
            RequestId(1),
            Generation(2),
            &target.item_id,
            sense_model::DocumentationState::Unresolved,
        ));
        state.highest_generation = 3;
        assert!(state.documentation_resolve_target().is_none());
    }

    #[test]
    fn adapter_resolution_can_upgrade_existing_native_documentation_once() {
        let session_id = SessionId::new();
        let mut state = BridgeState::new(
            NativeShell::Zsh,
            session_id,
            sense_protocol::DEFAULT_MAX_FRAME_BYTES,
            CaptureLimits::default(),
            2,
            GhostTextPolicy::default(),
            DocumentationPolicy::default(),
        )
        .unwrap();
        state.highest_generation = 2;
        let mut items = batch(1).items;
        items[0].documentation =
            sense_model::DocumentationState::Resolved(sense_model::MarkupContent {
                kind: sense_model::MarkupKind::PlainText,
                value: "Native summary".into(),
            });
        items[0]
            .capabilities
            .insert(sense_model::ItemCapabilities::RESOLVE_DOCUMENTATION);
        state.install_view(CandidateView {
            session_id,
            request_id: RequestId(1),
            generation: Generation(2),
            revision: 1,
            items,
            selected_index: Some(0),
            matched_before_limit: 1,
            sources_pending: Vec::new(),
            is_final: true,
            is_incomplete: false,
            is_settled: true,
        });

        let target = state.documentation_resolve_target().unwrap();
        assert!(state.install_documentation(
            RequestId(1),
            Generation(2),
            &target.item_id,
            sense_model::DocumentationState::Resolved(sense_model::MarkupContent {
                kind: sense_model::MarkupKind::PlainText,
                value: "Adapter documentation".into(),
            }),
        ));
        assert!(state.documentation_resolve_target().is_none());
    }
}
