use std::collections::BTreeMap;

use futures_util::{SinkExt, StreamExt};
use sense_daemon::{Server, ServerConfig};
use sense_model::{
    AdapterEvent, ByteOffset, CompletionItem, CompletionKind, CompletionRequest, ContextEpoch,
    DocumentationState, Enrichment, Generation, ItemId, ItemTags, MarkupContent, MarkupKind,
    NativeCommandContext, NativeShell, RawBytes, RequestId, TerminalDimensions, TextEdit,
    TextRange, TriggerKind,
};
use sense_protocol::{
    AdapterEventPublication, ClientHello, ClientMessage, MessagePackCodec,
    NativeContextPublication, PeerRole, ProtocolVersion, ResolveRequest, SelectionRequest,
    SelectionResult, ServerMessage, ShellIdentity,
};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::UnixStream;
use tokio::sync::oneshot;
use tokio_util::codec::Framed;

type TestClient = Framed<UnixStream, MessagePackCodec<ServerMessage, ClientMessage>>;

fn hello(role: PeerRole, attach_session: Option<sense_model::SessionId>) -> ClientMessage {
    hello_for(role, attach_session, NativeShell::Zsh)
}

fn hello_for(
    role: PeerRole,
    attach_session: Option<sense_model::SessionId>,
    shell: NativeShell,
) -> ClientMessage {
    ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "test".into(),
        role,
        process_id: std::process::id(),
        shell: Some(ShellIdentity {
            shell,
            executable: format!("/bin/{}", shell.source_name()),
            version: "5.9".into(),
            patchlevel: None,
        }),
        attach_session,
        attach_process_id: None,
    })
}

fn presentation_hello(shell_process_id: u32) -> ClientMessage {
    ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "test-presenter".into(),
        role: PeerRole::PresentationClient,
        process_id: std::process::id(),
        shell: None,
        attach_session: None,
        attach_process_id: Some(shell_process_id),
    })
}

fn systemctl_candidates(request: &CompletionRequest) -> sense_protocol::CandidateBatch {
    let labels = ["reset-failed", "restart"];
    sense_protocol::CandidateBatch {
        session_id: request.session_id,
        request_id: request.request_id,
        generation: request.generation,
        source: sense_model::SourceId("zsh".into()),
        items: labels
            .into_iter()
            .map(|label| {
                let mut item = CompletionItem::native(
                    label,
                    NativeShell::Zsh,
                    label,
                    TextEdit::new(TextRange::new(10, request.cursor.0), label),
                    label,
                );
                if label == "restart" {
                    item.documentation = DocumentationState::Resolved(MarkupContent {
                        kind: MarkupKind::PlainText,
                        value: "Restart one or more units.".into(),
                    });
                }
                item
            })
            .collect(),
        is_final: true,
        is_incomplete: false,
    }
}

async fn assert_resolved_documentation(client: &mut TestClient) {
    loop {
        match client.next().await.unwrap().unwrap() {
            ServerMessage::Documentation {
                item_id,
                documentation: DocumentationState::Resolved(MarkupContent { value, .. }),
                ..
            } => {
                assert_eq!(item_id, ItemId("restart".into()));
                assert_eq!(value, "Restart one or more units.");
                return;
            }
            ServerMessage::Error { code, message, .. } => {
                panic!("resolve failed with {code}: {message}");
            }
            _ => {}
        }
    }
}

async fn receive_until(
    client: &mut TestClient,
    predicate: impl Fn(&ServerMessage) -> bool,
) -> ServerMessage {
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let message = client.next().await.unwrap().unwrap();
            if predicate(&message) {
                return message;
            }
        }
    })
    .await
    .expect("timed out waiting for daemon event")
}

struct AdapterTestSession {
    _temporary: tempfile::TempDir,
    client: TestClient,
    worker: TestClient,
    adapter: TestClient,
    request: CompletionRequest,
    shutdown: oneshot::Sender<()>,
    task: tokio::task::JoinHandle<Result<(), sense_daemon::DaemonError>>,
}

impl AdapterTestSession {
    async fn start() -> Self {
        let temporary = tempfile::tempdir().unwrap();
        let socket = temporary.path().join("daemon.sock");
        let server = Server::bind(ServerConfig::new(&socket)).unwrap();
        let (shutdown, shutdown_rx) = oneshot::channel();
        let task = tokio::spawn(server.run_until(async {
            let _ = shutdown_rx.await;
        }));

        let mut client = Framed::new(
            UnixStream::connect(&socket).await.unwrap(),
            MessagePackCodec::<ServerMessage, ClientMessage>::default(),
        );
        client
            .send(hello(PeerRole::ShellClient, None))
            .await
            .unwrap();
        let ServerMessage::Welcome(welcome) = client.next().await.unwrap().unwrap() else {
            panic!("expected welcome");
        };

        let mut worker =
            Self::connect_peer(&socket, PeerRole::CompletionWorker, welcome.session_id).await;
        let adapter = Self::connect_peer(&socket, PeerRole::Adapter, welcome.session_id).await;
        let request = CompletionRequest {
            session_id: welcome.session_id,
            request_id: RequestId(9),
            generation: Generation(13),
            context_epoch: ContextEpoch::default(),
            buffer: RawBytes::from("systemctl "),
            cursor: ByteOffset(10),
            cwd: RawBytes::from("/tmp"),
            keymap: "emacs".into(),
            terminal: TerminalDimensions::default(),
            trigger: TriggerKind::Automatic,
            environment: BTreeMap::new(),
        };
        client
            .send(ClientMessage::Complete(request.clone()))
            .await
            .unwrap();
        receive_until(&mut worker, |message| {
            matches!(message, ServerMessage::CompletionRequested(received) if received == &request)
        })
        .await;

        Self {
            _temporary: temporary,
            client,
            worker,
            adapter,
            request,
            shutdown,
            task,
        }
    }

    async fn connect_peer(
        socket: &std::path::Path,
        role: PeerRole,
        session_id: sense_model::SessionId,
    ) -> TestClient {
        let mut peer = Framed::new(
            UnixStream::connect(socket).await.unwrap(),
            MessagePackCodec::<ServerMessage, ClientMessage>::default(),
        );
        peer.send(hello(role, Some(session_id))).await.unwrap();
        assert!(matches!(
            peer.next().await.unwrap().unwrap(),
            ServerMessage::Welcome(_)
        ));
        peer
    }

    async fn finish(self) {
        let Self { shutdown, task, .. } = self;
        shutdown.send(()).unwrap();
        task.await.unwrap().unwrap();
    }
}

#[tokio::test]
async fn session_streams_completion_requests_and_candidate_lifecycle() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let stream = UnixStream::connect(&socket).await.unwrap();
    let codec = MessagePackCodec::<ServerMessage, ClientMessage>::default();
    let mut client = Framed::new(stream, codec);
    client
        .send(hello(PeerRole::ShellClient, None))
        .await
        .unwrap();
    let ServerMessage::Welcome(welcome) = client.next().await.unwrap().unwrap() else {
        panic!("expected welcome");
    };

    let worker_stream = UnixStream::connect(&socket).await.unwrap();
    let worker_codec = MessagePackCodec::<ServerMessage, ClientMessage>::default();
    let mut worker = Framed::new(worker_stream, worker_codec);
    worker
        .send(hello(PeerRole::CompletionWorker, Some(welcome.session_id)))
        .await
        .unwrap();
    assert!(matches!(
        worker.next().await.unwrap().unwrap(),
        ServerMessage::Welcome(_)
    ));

    let request = CompletionRequest {
        session_id: welcome.session_id,
        request_id: RequestId(7),
        generation: Generation(11),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from(b"systemctl rstart".as_slice()),
        cursor: ByteOffset(16),
        cwd: RawBytes::from(b"/tmp".as_slice()),
        keymap: "emacs".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::new(),
    };
    client
        .send(ClientMessage::Complete(request.clone()))
        .await
        .unwrap();

    let mut worker_saw_request = false;
    for _ in 0..2 {
        if matches!(
            worker.next().await.unwrap().unwrap(),
            ServerMessage::CompletionRequested(ref received) if received == &request
        ) {
            worker_saw_request = true;
        }
    }
    assert!(worker_saw_request);

    worker
        .send(ClientMessage::PublishCandidates(systemctl_candidates(
            &request,
        )))
        .await
        .unwrap();

    let mut saw_candidates = false;
    let mut saw_finished = false;
    for _ in 0..4 {
        match client.next().await.unwrap().unwrap() {
            ServerMessage::CandidateView(view) => {
                saw_candidates = true;
                assert!(view.is_final);
                assert!(view.sources_pending.is_empty());
                assert_eq!(view.items[0].label, "restart");
            }
            ServerMessage::RequestFinished {
                cancelled: false, ..
            } => saw_finished = true,
            _ => {}
        }
        if saw_candidates && saw_finished {
            break;
        }
    }
    assert!(saw_candidates && saw_finished);

    client
        .send(ClientMessage::Resolve(ResolveRequest {
            session_id: welcome.session_id,
            request_id: request.request_id,
            generation: request.generation,
            item_id: ItemId("restart".into()),
        }))
        .await
        .unwrap();
    assert_resolved_documentation(&mut client).await;

    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
    assert!(!socket.exists());
}

#[tokio::test]
async fn worker_cannot_create_an_unattached_session() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let stream = UnixStream::connect(&socket).await.unwrap();
    let codec = MessagePackCodec::<ServerMessage, ClientMessage>::default();
    let mut client = Framed::new(stream, codec);
    client
        .send(hello(PeerRole::CompletionWorker, None))
        .await
        .unwrap();
    assert!(matches!(
        client.next().await.unwrap().unwrap(),
        ServerMessage::Error { .. }
    ));

    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

#[tokio::test]
async fn session_rejects_a_worker_from_another_native_shell() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let mut client = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    client
        .send(hello(PeerRole::ShellClient, None))
        .await
        .unwrap();
    let ServerMessage::Welcome(welcome) = client.next().await.unwrap().unwrap() else {
        panic!("expected welcome");
    };

    let mut worker = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    worker
        .send(hello_for(
            PeerRole::CompletionWorker,
            Some(welcome.session_id),
            NativeShell::Fish,
        ))
        .await
        .unwrap();
    assert!(matches!(
        worker.next().await.unwrap().unwrap(),
        ServerMessage::Error { ref code, .. } if code == "handshake-failed"
    ));

    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

async fn attach_presenter_and_assert_snapshot(
    socket: &std::path::Path,
    shell: &mut TestClient,
    session_id: sense_model::SessionId,
    request: &CompletionRequest,
) -> TestClient {
    let mut presenter = Framed::new(
        UnixStream::connect(socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    presenter
        .send(presentation_hello(std::process::id()))
        .await
        .unwrap();
    let ServerMessage::Welcome(presenter_welcome) = presenter.next().await.unwrap().unwrap() else {
        panic!("expected presentation welcome");
    };
    assert_eq!(presenter_welcome.session_id, session_id);
    assert_eq!(
        receive_until(shell, |message| matches!(
            message,
            ServerMessage::PresentationChanged { external: true }
        ))
        .await,
        ServerMessage::PresentationChanged { external: true }
    );
    receive_until(&mut presenter, |message| {
        matches!(message, ServerMessage::CompletionRequested(received) if received == request)
    })
    .await;
    receive_until(&mut presenter, |message| {
        matches!(message, ServerMessage::CandidateView(view) if view.request_id == request.request_id)
    })
    .await;
    presenter
}

async fn request_native_selection(
    presenter: &mut TestClient,
    worker: &mut TestClient,
    selection: &SelectionRequest,
) {
    presenter
        .send(ClientMessage::Select(selection.clone()))
        .await
        .unwrap();
    receive_until(worker, |message| {
        matches!(message, ServerMessage::SelectionRequested(received) if received == selection)
    })
    .await;
}

async fn assert_worker_rejection(
    presenter: &mut TestClient,
    worker: &mut TestClient,
    selection: &SelectionRequest,
) {
    request_native_selection(presenter, worker, selection).await;
    worker
        .send(ClientMessage::ReportSelection(SelectionResult {
            selection: selection.clone(),
            applied: false,
        }))
        .await
        .unwrap();
    assert_eq!(
        receive_until(presenter, |message| matches!(
            message,
            ServerMessage::SelectionFinished(result)
                if &result.selection == selection && !result.applied
        ))
        .await,
        ServerMessage::SelectionFinished(SelectionResult {
            selection: selection.clone(),
            applied: false,
        })
    );
}

async fn assert_worker_cannot_report_application(
    shell: &mut TestClient,
    presenter: &mut TestClient,
    worker: &mut TestClient,
    selection: &SelectionRequest,
) {
    request_native_selection(presenter, worker, selection).await;
    worker
        .send(ClientMessage::ReportSelection(SelectionResult {
            selection: selection.clone(),
            applied: true,
        }))
        .await
        .unwrap();
    receive_until(worker, |message| {
        matches!(
            message,
            ServerMessage::Error { code, .. } if code == "role-not-authorized"
        )
    })
    .await;
    shell
        .send(ClientMessage::ReportSelection(SelectionResult {
            selection: selection.clone(),
            applied: false,
        }))
        .await
        .unwrap();
    receive_until(presenter, |message| {
        matches!(
            message,
            ServerMessage::SelectionFinished(result)
                if &result.selection == selection && !result.applied
        )
    })
    .await;
}

#[tokio::test]
async fn presentation_client_attaches_by_shell_pid_and_uses_native_acceptance() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let mut shell = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    shell
        .send(hello(PeerRole::ShellClient, None))
        .await
        .unwrap();
    let ServerMessage::Welcome(welcome) = shell.next().await.unwrap().unwrap() else {
        panic!("expected shell welcome");
    };
    let mut worker =
        AdapterTestSession::connect_peer(&socket, PeerRole::CompletionWorker, welcome.session_id)
            .await;
    let request = CompletionRequest {
        session_id: welcome.session_id,
        request_id: RequestId(41),
        generation: Generation(17),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from("systemctl r"),
        cursor: ByteOffset(11),
        cwd: RawBytes::from("/tmp"),
        keymap: "emacs".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::new(),
    };
    shell
        .send(ClientMessage::Complete(request.clone()))
        .await
        .unwrap();
    receive_until(&mut worker, |message| {
        matches!(message, ServerMessage::CompletionRequested(received) if received == &request)
    })
    .await;
    worker
        .send(ClientMessage::PublishCandidates(systemctl_candidates(
            &request,
        )))
        .await
        .unwrap();
    receive_until(&mut shell, |message| {
        matches!(message, ServerMessage::CandidateView(_))
    })
    .await;

    let mut presenter =
        attach_presenter_and_assert_snapshot(&socket, &mut shell, welcome.session_id, &request)
            .await;

    let selection = SelectionRequest {
        session_id: welcome.session_id,
        request_id: request.request_id,
        generation: request.generation,
        item_id: ItemId("restart".into()),
    };
    request_native_selection(&mut presenter, &mut worker, &selection).await;
    shell
        .send(ClientMessage::ReportSelection(SelectionResult {
            selection: selection.clone(),
            applied: true,
        }))
        .await
        .unwrap();
    assert_eq!(
        receive_until(&mut presenter, |message| matches!(
            message,
            ServerMessage::SelectionFinished(result) if result.selection == selection
        ))
        .await,
        ServerMessage::SelectionFinished(SelectionResult {
            selection: selection.clone(),
            applied: true,
        })
    );

    assert_worker_rejection(&mut presenter, &mut worker, &selection).await;
    assert_worker_cannot_report_application(&mut shell, &mut presenter, &mut worker, &selection)
        .await;

    presenter.send(ClientMessage::Goodbye).await.unwrap();
    assert_eq!(
        receive_until(&mut shell, |message| matches!(
            message,
            ServerMessage::PresentationChanged { external: false }
        ))
        .await,
        ServerMessage::PresentationChanged { external: false }
    );
    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

#[tokio::test]
async fn presentation_client_rejects_an_unknown_shell_pid() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));
    let mut presenter = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    presenter.send(presentation_hello(u32::MAX)).await.unwrap();
    assert!(matches!(
        presenter.next().await.unwrap().unwrap(),
        ServerMessage::Error { ref code, .. } if code == "shell-session-unavailable"
    ));
    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

#[tokio::test]
async fn shell_session_is_removed_after_a_protocol_error() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let mut shell = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    shell
        .send(hello(PeerRole::ShellClient, None))
        .await
        .unwrap();
    assert!(matches!(
        shell.next().await.unwrap().unwrap(),
        ServerMessage::Welcome(_)
    ));

    let mut attached_presenter = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    attached_presenter
        .send(presentation_hello(std::process::id()))
        .await
        .unwrap();
    assert!(matches!(
        attached_presenter.next().await.unwrap().unwrap(),
        ServerMessage::Welcome(_)
    ));

    let mut raw_shell = shell.into_inner();
    raw_shell.write_all(&[0, 0, 0, 1, 0xc1]).await.unwrap();
    let mut trailing = Vec::new();
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        raw_shell.read_to_end(&mut trailing),
    )
    .await
    .expect("daemon did not close the invalid shell connection")
    .unwrap();
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        while attached_presenter.next().await.is_some() {}
    })
    .await
    .expect("daemon did not close the attached presentation client");

    let mut presenter = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    presenter
        .send(presentation_hello(std::process::id()))
        .await
        .unwrap();
    assert!(matches!(
        presenter.next().await.unwrap().unwrap(),
        ServerMessage::Error { ref code, .. } if code == "shell-session-unavailable"
    ));

    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

#[tokio::test]
async fn context_adapter_cannot_publish_candidates() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let mut client = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    client
        .send(hello(PeerRole::ShellClient, None))
        .await
        .unwrap();
    let ServerMessage::Welcome(welcome) = client.next().await.unwrap().unwrap() else {
        panic!("expected welcome");
    };

    let mut adapter = Framed::new(
        UnixStream::connect(&socket).await.unwrap(),
        MessagePackCodec::<ServerMessage, ClientMessage>::default(),
    );
    adapter
        .send(hello(PeerRole::Adapter, Some(welcome.session_id)))
        .await
        .unwrap();
    assert!(matches!(
        adapter.next().await.unwrap().unwrap(),
        ServerMessage::Welcome(_)
    ));

    let request = CompletionRequest {
        session_id: welcome.session_id,
        request_id: RequestId(1),
        generation: Generation(1),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from("git che"),
        cursor: ByteOffset(7),
        cwd: RawBytes::from("/tmp"),
        keymap: "default".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::new(),
    };
    adapter
        .send(ClientMessage::PublishCandidates(systemctl_candidates(
            &request,
        )))
        .await
        .unwrap();
    assert!(matches!(
        adapter.next().await.unwrap().unwrap(),
        ServerMessage::Error { ref code, .. } if code == "role-not-authorized"
    ));

    shutdown_tx.send(()).unwrap();
    task.await.unwrap().unwrap();
}

#[tokio::test]
async fn adapter_events_only_enrich_current_native_items() {
    let mut session = AdapterTestSession::start().await;
    publish_native_context_and_candidates(&mut session).await;
    publish_and_assert_enrichments(&mut session).await;
    publish_and_assert_documentation(&mut session).await;
    assert_adapter_role_is_required(&mut session).await;
    session.finish().await;
}

#[tokio::test]
async fn built_in_adapter_enriches_without_changing_native_authority() {
    let mut session = AdapterTestSession::start().await;
    publish_native_context_and_candidates(&mut session).await;

    let ServerMessage::CandidateView(view) = receive_until(&mut session.client, |message| {
        matches!(message, ServerMessage::CandidateView(view) if view.items.iter().all(|item| {
            item.kind == CompletionKind::Subcommand
                && item
                    .capabilities
                    .contains(sense_model::ItemCapabilities::RESOLVE_DOCUMENTATION)
        }))
    })
    .await
    else {
        unreachable!();
    };
    assert_eq!(view.items.len(), 2);
    assert!(view.items.iter().all(|item| {
        item.source.0 == "zsh"
            && matches!(
                item.insertion,
                sense_model::InsertStrategy::NativeMatch {
                    shell: NativeShell::Zsh,
                    ..
                }
            )
    }));

    session.finish().await;
}

async fn publish_native_context_and_candidates(session: &mut AdapterTestSession) {
    let native_context = NativeContextPublication {
        session_id: session.request.session_id,
        request_id: session.request.request_id,
        generation: session.request.generation,
        context: NativeCommandContext {
            words: vec![RawBytes::from("systemctl"), RawBytes::default()],
            current_word: Some(1),
        },
    };
    session
        .worker
        .send(ClientMessage::PublishNativeContext(native_context.clone()))
        .await
        .unwrap();
    assert_eq!(
        receive_until(&mut session.adapter, |message| {
            matches!(message, ServerMessage::NativeContextPublished(_))
        })
        .await,
        ServerMessage::NativeContextPublished(native_context)
    );

    session
        .worker
        .send(ClientMessage::PublishCandidates(systemctl_candidates(
            &session.request,
        )))
        .await
        .unwrap();
    receive_until(&mut session.adapter, |message| {
        matches!(message, ServerMessage::CandidateView(_))
    })
    .await;
}

async fn publish_and_assert_enrichments(session: &mut AdapterTestSession) {
    session
        .adapter
        .send(ClientMessage::PublishAdapterEvent(
            AdapterEventPublication {
                session_id: session.request.session_id,
                request_id: session.request.request_id,
                generation: session.request.generation,
                event: AdapterEvent::Enrichments(vec![
                    Enrichment {
                        item_id: ItemId("restart".into()),
                        kind: Some(CompletionKind::Subcommand),
                        add_tags: ItemTags::RUNNING,
                        add_capabilities: sense_model::ItemCapabilities::RESOLVE_DOCUMENTATION,
                        detail: Some("running systemd operation".into()),
                        documentation: Some(DocumentationState::Unresolved),
                    },
                    Enrichment {
                        item_id: ItemId("adapter-invented".into()),
                        kind: Some(CompletionKind::Command),
                        add_tags: ItemTags::empty(),
                        add_capabilities: sense_model::ItemCapabilities::empty(),
                        detail: Some("must never become a candidate".into()),
                        documentation: None,
                    },
                ]),
            },
        ))
        .await
        .unwrap();

    let ServerMessage::CandidateView(view) = receive_until(&mut session.client, |message| {
        matches!(message, ServerMessage::CandidateView(view) if view.items.iter().any(|item| {
            item.id.0 == "restart"
                && item.detail.as_deref() == Some("running systemd operation")
        }))
    })
    .await
    else {
        unreachable!();
    };
    assert_eq!(view.items.len(), 2);
    assert!(
        view.items
            .iter()
            .all(|item| item.id.0 != "adapter-invented")
    );
    let restart = view
        .items
        .iter()
        .find(|item| item.id.0 == "restart")
        .unwrap();
    assert_eq!(restart.kind, CompletionKind::Subcommand);
    assert!(restart.tags.contains(ItemTags::RUNNING));
    assert_eq!(restart.documentation, DocumentationState::Unresolved);
}

async fn publish_and_assert_documentation(session: &mut AdapterTestSession) {
    let documentation = DocumentationState::Resolved(MarkupContent {
        kind: MarkupKind::Markdown,
        value: "# restart\n\nRestart matching units.".into(),
    });
    session
        .adapter
        .send(ClientMessage::PublishAdapterEvent(
            AdapterEventPublication {
                session_id: session.request.session_id,
                request_id: session.request.request_id,
                generation: session.request.generation,
                event: AdapterEvent::Documentation {
                    item_id: ItemId("restart".into()),
                    documentation: documentation.clone(),
                },
            },
        ))
        .await
        .unwrap();
    assert!(matches!(
        receive_until(&mut session.client, |message| {
            matches!(message, ServerMessage::Documentation { item_id, documentation: received, .. }
                if item_id.0 == "restart" && received == &documentation)
        })
        .await,
        ServerMessage::Documentation { .. }
    ));
}

async fn assert_adapter_role_is_required(session: &mut AdapterTestSession) {
    session
        .client
        .send(ClientMessage::PublishAdapterEvent(
            AdapterEventPublication {
                session_id: session.request.session_id,
                request_id: session.request.request_id,
                generation: session.request.generation,
                event: AdapterEvent::Enrichments(Vec::new()),
            },
        ))
        .await
        .unwrap();
    assert!(matches!(
        receive_until(&mut session.client, |message| matches!(
            message,
            ServerMessage::Error { code, .. } if code == "role-not-authorized"
        ))
        .await,
        ServerMessage::Error { .. }
    ));
}
