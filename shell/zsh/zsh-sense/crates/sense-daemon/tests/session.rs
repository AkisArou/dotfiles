use std::collections::BTreeMap;

use futures_util::{SinkExt, StreamExt};
use sense_daemon::{Server, ServerConfig};
use sense_model::{
    ByteOffset, CompletionItem, CompletionRequest, ContextEpoch, Generation, RawBytes, RequestId,
    TerminalDimensions, TextEdit, TextRange, TriggerKind,
};
use sense_protocol::{
    ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolVersion, ServerMessage,
};
use tokio::net::UnixStream;
use tokio::sync::oneshot;
use tokio_util::codec::Framed;

fn hello(role: PeerRole, attach_session: Option<sense_model::SessionId>) -> ClientMessage {
    ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "test".into(),
        role,
        process_id: std::process::id(),
        zsh: None,
        attach_session,
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
                CompletionItem::plain(
                    label,
                    "zsh",
                    label,
                    TextEdit::new(TextRange::new(10, 16), label),
                )
            })
            .collect(),
        is_final: true,
        is_incomplete: false,
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
    client.send(hello(PeerRole::ZleClient, None)).await.unwrap();
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
