//! Blink.cmp presentation bridge for a live Shell Sense session.
//!
//! The bridge is deliberately presentation-only. It observes daemon-ranked
//! native candidates, asks the daemon to resolve documentation, and routes
//! selection back to the owning shell. It never inserts completion text.

use std::path::PathBuf;
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use sense_model::{
    CompletionItem, CompletionKind, DocumentationState, Generation, ItemId, ItemTags,
    MarkupContent, MarkupKind, RequestId, SessionId, TriggerKind,
};
use sense_protocol::{
    CandidateView, ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolError,
    ProtocolVersion, ResolveRequest, SelectionRequest, ServerMessage,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader, BufWriter};
use tokio::net::UnixStream;
use tokio_util::codec::Framed;

type DaemonConnection = Framed<UnixStream, MessagePackCodec<ServerMessage, ClientMessage>>;

#[derive(Debug, Clone)]
pub struct BridgeConfig {
    pub socket_path: PathBuf,
    pub shell_process_id: u32,
    pub attach_timeout: Duration,
}

impl BridgeConfig {
    #[must_use]
    pub fn new(socket_path: impl Into<PathBuf>, shell_process_id: u32) -> Self {
        Self {
            socket_path: socket_path.into(),
            shell_process_id,
            attach_timeout: Duration::from_secs(3),
        }
    }
}

#[derive(Debug, Error)]
pub enum BridgeError {
    #[error("Blink bridge I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("Blink bridge daemon protocol failed: {0}")]
    Protocol(#[from] ProtocolError),
    #[error("Blink bridge JSON failed: {0}")]
    Json(#[from] serde_json::Error),
    #[error("daemon closed the presentation connection during handshake")]
    HandshakeClosed,
    #[error("daemon rejected presentation attachment: {code}: {message}")]
    HandshakeRejected { code: String, message: String },
    #[error("daemon sent {0} instead of a presentation welcome")]
    UnexpectedHandshake(&'static str),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case", deny_unknown_fields)]
pub enum EditorCommand {
    Select {
        request_id: u64,
        generation: u64,
        item_id: String,
    },
    Resolve {
        request_id: u64,
        generation: u64,
        item_id: String,
    },
    Goodbye,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum EditorEvent {
    Ready {
        session_id: SessionId,
    },
    Request {
        request_id: u64,
        generation: u64,
        command: Option<String>,
        cursor: u32,
        trigger: TriggerKind,
    },
    Completions(PresentationList),
    Documentation {
        request_id: u64,
        generation: u64,
        item_id: String,
        documentation: Option<PresentationMarkup>,
        unresolved: bool,
    },
    RequestCancelled {
        request_id: u64,
        generation: u64,
    },
    SelectionFinished {
        request_id: u64,
        generation: u64,
        item_id: String,
        applied: bool,
    },
    Error {
        code: String,
        message: String,
        request_id: Option<u64>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationList {
    pub request_id: u64,
    pub generation: u64,
    pub revision: u64,
    pub items: Vec<PresentationItem>,
    pub selected_index: Option<u32>,
    pub matched_before_limit: u32,
    pub is_final: bool,
    pub is_incomplete: bool,
    pub is_settled: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationItem {
    pub id: String,
    pub label: String,
    pub label_detail: Option<String>,
    pub filter_text: Option<String>,
    pub sort_text: String,
    pub kind: CompletionKind,
    pub lsp_kind: u8,
    pub deprecated: bool,
    pub detail: Option<String>,
    pub documentation: Option<PresentationMarkup>,
    pub documentation_unresolved: bool,
    pub source: String,
    pub group: Option<String>,
    pub edit: PresentationEdit,
    pub matched: Option<PresentationMatch>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationEdit {
    /// Byte offsets relative to the shell command, not the terminal row.
    pub start: u32,
    pub end: u32,
    /// Display-only fallback. Acceptance always uses the native shell route.
    pub display_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationMatch {
    pub score: i64,
    pub indices: Vec<u32>,
    pub exact: bool,
    pub prefix: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationMarkup {
    pub kind: MarkupKind,
    pub value: String,
}

impl From<MarkupContent> for PresentationMarkup {
    fn from(content: MarkupContent) -> Self {
        Self {
            kind: content.kind,
            value: content.value,
        }
    }
}

/// Attach to the live shell session and serve newline-delimited JSON on
/// standard input/output.
///
/// # Errors
///
/// Returns when attachment, daemon framing, JSON, or standard I/O fails.
pub async fn run(config: BridgeConfig) -> Result<(), BridgeError> {
    let deadline = tokio::time::Instant::now() + config.attach_timeout;
    let (daemon, session_id) = loop {
        match attach_once(&config).await {
            Ok(attached) => break attached,
            Err(BridgeError::HandshakeRejected { code, .. })
                if code == "shell-session-unavailable"
                    && tokio::time::Instant::now() < deadline => {}
            Err(BridgeError::Io(error))
                if matches!(
                    error.kind(),
                    std::io::ErrorKind::NotFound | std::io::ErrorKind::ConnectionRefused
                ) && tokio::time::Instant::now() < deadline => {}
            Err(error) => return Err(error),
        }
        tokio::time::sleep(Duration::from_millis(25)).await;
    };

    let input = BufReader::new(tokio::io::stdin());
    let output = BufWriter::new(tokio::io::stdout());
    run_session(daemon, session_id, input, output).await
}

async fn attach_once(config: &BridgeConfig) -> Result<(DaemonConnection, SessionId), BridgeError> {
    let connection = UnixStream::connect(&config.socket_path).await?;
    let mut daemon = Framed::new(
        connection,
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    daemon
        .send(ClientMessage::Hello(ClientHello {
            protocol: ProtocolVersion::CURRENT,
            client_version: env!("CARGO_PKG_VERSION").into(),
            role: PeerRole::PresentationClient,
            process_id: std::process::id(),
            shell: None,
            attach_session: None,
            attach_process_id: Some(config.shell_process_id),
        }))
        .await?;
    let welcome = match daemon.next().await {
        Some(Ok(ServerMessage::Welcome(welcome))) => welcome,
        Some(Ok(ServerMessage::Error { code, message, .. })) => {
            return Err(BridgeError::HandshakeRejected { code, message });
        }
        Some(Ok(message)) => return Err(BridgeError::UnexpectedHandshake(message_name(&message))),
        Some(Err(error)) => return Err(error.into()),
        None => return Err(BridgeError::HandshakeClosed),
    };

    Ok((daemon, welcome.session_id))
}

async fn run_session<R, W>(
    mut daemon: DaemonConnection,
    session_id: SessionId,
    input: R,
    mut output: W,
) -> Result<(), BridgeError>
where
    R: AsyncBufRead + Unpin,
    W: AsyncWrite + Unpin,
{
    send_event(&mut output, &EditorEvent::Ready { session_id }).await?;
    let mut lines = input.lines();
    loop {
        tokio::select! {
            line = lines.next_line() => {
                let Some(line) = line? else {
                    daemon.send(ClientMessage::Goodbye).await.ok();
                    return Ok(());
                };
                let command = match serde_json::from_str::<EditorCommand>(&line) {
                    Ok(command) => command,
                    Err(error) => {
                        send_event(&mut output, &EditorEvent::Error {
                            code: "invalid-editor-command".into(),
                            message: error.to_string(),
                            request_id: None,
                        }).await?;
                        continue;
                    }
                };
                if send_editor_command(&mut daemon, session_id, command).await? {
                    return Ok(());
                }
            }
            message = daemon.next() => {
                let Some(message) = message else { return Ok(()) };
                if let Some(event) = presentation_event(message?) {
                    send_event(&mut output, &event).await?;
                }
            }
        }
    }
}

async fn send_editor_command(
    daemon: &mut DaemonConnection,
    session_id: SessionId,
    command: EditorCommand,
) -> Result<bool, ProtocolError> {
    match command {
        EditorCommand::Select {
            request_id,
            generation,
            item_id,
        } => {
            daemon
                .send(ClientMessage::Select(SelectionRequest {
                    session_id,
                    request_id: RequestId(request_id),
                    generation: Generation(generation),
                    item_id: ItemId(item_id),
                }))
                .await?;
        }
        EditorCommand::Resolve {
            request_id,
            generation,
            item_id,
        } => {
            daemon
                .send(ClientMessage::Resolve(ResolveRequest {
                    session_id,
                    request_id: RequestId(request_id),
                    generation: Generation(generation),
                    item_id: ItemId(item_id),
                }))
                .await?;
        }
        EditorCommand::Goodbye => {
            daemon.send(ClientMessage::Goodbye).await?;
            return Ok(true);
        }
    }
    Ok(false)
}

async fn send_event<W>(output: &mut W, event: &EditorEvent) -> Result<(), BridgeError>
where
    W: AsyncWrite + Unpin,
{
    let encoded = serde_json::to_vec(event)?;
    output.write_all(&encoded).await?;
    output.write_all(b"\n").await?;
    output.flush().await?;
    Ok(())
}

fn presentation_event(message: ServerMessage) -> Option<EditorEvent> {
    match message {
        ServerMessage::CompletionRequested(request) => Some(EditorEvent::Request {
            request_id: request.request_id.0,
            generation: request.generation.0,
            command: String::from_utf8(request.buffer.0).ok(),
            cursor: request.cursor.0,
            trigger: request.trigger,
        }),
        ServerMessage::CandidateView(view) => {
            Some(EditorEvent::Completions(presentation_list(view)))
        }
        ServerMessage::Documentation {
            request_id,
            generation,
            item_id,
            documentation,
        } => {
            let (documentation, unresolved) = presentation_documentation(documentation);
            Some(EditorEvent::Documentation {
                request_id: request_id.0,
                generation: generation.0,
                item_id: item_id.0,
                documentation,
                unresolved,
            })
        }
        ServerMessage::RequestCancelled {
            request_id,
            generation,
        } => Some(EditorEvent::RequestCancelled {
            request_id: request_id.0,
            generation: generation.0,
        }),
        ServerMessage::SelectionFinished(result) => Some(EditorEvent::SelectionFinished {
            request_id: result.selection.request_id.0,
            generation: result.selection.generation.0,
            item_id: result.selection.item_id.0,
            applied: result.applied,
        }),
        ServerMessage::Error {
            code,
            message,
            request_id,
        } => Some(EditorEvent::Error {
            code,
            message,
            request_id: request_id.map(|id| id.0),
        }),
        ServerMessage::Welcome(_)
        | ServerMessage::NativeContextPublished(_)
        | ServerMessage::SelectionRequested(_)
        | ServerMessage::ResolveRequested(_)
        | ServerMessage::RequestStarted { .. }
        | ServerMessage::Candidates(_)
        | ServerMessage::RequestFinished { .. }
        | ServerMessage::PresentationChanged { .. }
        | ServerMessage::Pong { .. } => None,
    }
}

fn presentation_list(view: CandidateView) -> PresentationList {
    let items = view
        .items
        .into_iter()
        .enumerate()
        .map(|(index, item)| presentation_item(item, index))
        .collect();
    PresentationList {
        request_id: view.request_id.0,
        generation: view.generation.0,
        revision: view.revision,
        items,
        selected_index: view.selected_index,
        matched_before_limit: view.matched_before_limit,
        is_final: view.is_final,
        is_incomplete: view.is_incomplete,
        is_settled: view.is_settled,
    }
}

fn presentation_item(item: CompletionItem, index: usize) -> PresentationItem {
    let (documentation, documentation_unresolved) = presentation_documentation(item.documentation);
    let matched = item.match_result.map(|matched| PresentationMatch {
        score: matched.score,
        indices: matched.indices,
        exact: matched.exact,
        prefix: matched.prefix,
    });
    PresentationItem {
        id: item.id.0,
        label: item.label.clone(),
        label_detail: item.label_detail,
        filter_text: item.filter_text,
        sort_text: format!("{index:010}"),
        lsp_kind: lsp_completion_kind(item.kind),
        kind: item.kind,
        deprecated: item.tags.contains(ItemTags::DEPRECATED),
        detail: item.detail,
        documentation,
        documentation_unresolved,
        source: item.source.0,
        group: item.group.map(|group| group.0),
        edit: PresentationEdit {
            start: item.edit.range.start.0,
            end: item.edit.range.end.0,
            display_text: item.label,
        },
        matched,
    }
}

fn presentation_documentation(
    documentation: DocumentationState,
) -> (Option<PresentationMarkup>, bool) {
    match documentation {
        DocumentationState::None => (None, false),
        DocumentationState::Unresolved => (None, true),
        DocumentationState::Resolved(content) => (Some(content.into()), false),
    }
}

const fn lsp_completion_kind(kind: CompletionKind) -> u8 {
    match kind {
        CompletionKind::Text => 1,
        CompletionKind::Command
        | CompletionKind::Alias
        | CompletionKind::Builtin
        | CompletionKind::Function
        | CompletionKind::Subcommand => 3,
        CompletionKind::Option => 14,
        CompletionKind::OptionValue => 12,
        CompletionKind::Variable | CompletionKind::User => 6,
        CompletionKind::File | CompletionKind::Symlink => 17,
        CompletionKind::Directory => 19,
        CompletionKind::Host | CompletionKind::GitBranch | CompletionKind::GitCommit => 18,
        CompletionKind::Process | CompletionKind::Job => 23,
        CompletionKind::GitTag => 20,
        CompletionKind::Service
        | CompletionKind::Container
        | CompletionKind::Image
        | CompletionKind::Package => 9,
    }
}

const fn message_name(message: &ServerMessage) -> &'static str {
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
    use sense_model::{NativeShell, RawBytes, TextEdit, TextRange};

    use super::*;

    #[test]
    fn presentation_mapping_never_exposes_native_insertion_as_authority() {
        let mut item = CompletionItem::native(
            "native-id",
            NativeShell::Zsh,
            "checkout",
            TextEdit::new(TextRange::new(4, 7), RawBytes::new(vec![0xff, b'x'])),
            "opaque-native-fingerprint",
        );
        item.kind = CompletionKind::Subcommand;
        item.documentation = DocumentationState::Resolved(MarkupContent {
            kind: MarkupKind::Markdown,
            value: "Switch branches".into(),
        });
        item.match_result = Some(sense_model::MatchResult {
            score: 42,
            indices: vec![0, 2, 4],
            exact: false,
            prefix: false,
        });
        let mapped = presentation_item(item, 3);
        assert_eq!(mapped.id, "native-id");
        assert_eq!(mapped.edit.start, 4);
        assert_eq!(mapped.edit.end, 7);
        assert_eq!(mapped.edit.display_text, "checkout");
        assert_eq!(mapped.sort_text, "0000000003");
        assert_eq!(mapped.lsp_kind, 3);
        assert_eq!(mapped.matched.unwrap().indices, [0, 2, 4]);
        assert_eq!(mapped.documentation.unwrap().value, "Switch branches");
    }

    #[test]
    fn editor_commands_reject_unknown_fields() {
        assert!(
            serde_json::from_str::<EditorCommand>(
                r#"{"type":"resolve","request_id":1,"generation":2,"item_id":"x","extra":true}"#,
            )
            .is_err()
        );
    }
}
