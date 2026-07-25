use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use sense_daemon::{Server, ServerConfig};
use sense_model::RawBytes;
use sense_zsh_worker::{BridgeConfig, ShellWireCodec, ShellWireMessage, run_bridge};
use tokio::io::duplex;
use tokio::sync::oneshot;
use tokio::time::timeout;
use tokio_util::codec::{FramedRead, FramedWrite};

fn raw(value: impl Into<RawBytes>) -> RawBytes {
    value.into()
}

fn complete() -> ShellWireMessage {
    ShellWireMessage::new(
        "complete",
        vec![
            raw("7"),
            raw("11"),
            RawBytes::default(),
            raw("systemctl rstart"),
            raw("16"),
            raw("/tmp"),
            raw("emacs"),
            raw("120"),
            raw("40"),
            raw("automatic"),
            raw("0"),
        ],
    )
}

fn candidate(label: &str, description: &str, identity: &str, order: u32) -> ShellWireMessage {
    ShellWireMessage::new(
        "candidate",
        vec![
            raw("7"),
            raw("11"),
            raw(label),
            raw(label),
            raw(description),
            RawBytes::default(),
            raw("subcommands"),
            raw("systemctl commands"),
            raw("0"),
            raw("10"),
            raw("16"),
            raw("subcommand"),
            raw("0"),
            raw(identity),
            raw(order.to_string()),
            RawBytes::default(),
            raw(" "),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            RawBytes::default(),
            raw("0"),
        ],
    )
}

async fn next_command(
    reader: &mut FramedRead<tokio::io::DuplexStream, ShellWireCodec>,
    command: &str,
) -> ShellWireMessage {
    timeout(Duration::from_secs(5), async {
        loop {
            let message = reader.next().await.unwrap().unwrap();
            if message.command == command {
                break message;
            }
        }
    })
    .await
    .unwrap_or_else(|_| panic!("timed out waiting for {command}"))
}

#[tokio::test]
async fn bridge_streams_ranked_candidates_and_routes_selection() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let server_task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let (shell_to_bridge, bridge_input) = duplex(1024 * 1024);
    let (bridge_output, shell_from_bridge) = duplex(1024 * 1024);
    let bridge_task = tokio::spawn(run_bridge(
        BridgeConfig::new(&socket),
        bridge_input,
        bridge_output,
    ));
    let mut writer = FramedWrite::new(shell_to_bridge, ShellWireCodec::default());
    let mut reader = FramedRead::new(shell_from_bridge, ShellWireCodec::default());

    let ready = next_command(&mut reader, "ready").await;
    assert_eq!(ready.fields.len(), 4);
    writer.send(complete()).await.unwrap();

    let capture_request = next_command(&mut reader, "capture-request").await;
    assert_eq!(capture_request.fields[0].as_slice(), b"7");
    assert_eq!(capture_request.fields[2].as_slice(), b"systemctl rstart");

    writer
        .send(ShellWireMessage::new(
            "capture-begin",
            vec![raw("7"), raw("11"), raw("portable")],
        ))
        .await
        .unwrap();
    writer
        .send(candidate(
            "reset-failed",
            "Reset failed state",
            "replay-0",
            0,
        ))
        .await
        .unwrap();
    writer
        .send(candidate(
            "restart",
            "Restart one or more units",
            "replay-1",
            1,
        ))
        .await
        .unwrap();
    writer
        .send(ShellWireMessage::new(
            "capture-end",
            vec![raw("7"), raw("11")],
        ))
        .await
        .unwrap();

    let view_begin = next_command(&mut reader, "view-begin").await;
    assert_eq!(view_begin.fields[4].as_slice(), b"0");
    assert_eq!(view_begin.fields[8].as_slice(), b"1");
    let first = next_command(&mut reader, "view-chunk").await;
    assert_eq!(first.fields[2].as_slice(), b"1");
    assert_eq!(first.fields[4].as_slice(), b"restart");
    assert_eq!(first.fields[5].as_slice(), b"7");
    assert_eq!(first.fields[7].as_slice(), b"Restart one or more units");
    assert_eq!(first.fields[10].as_slice(), b"portable");
    assert_eq!(first.fields[11].as_slice(), b"replay-1");
    let selected_item_id = first.fields[3].clone();
    next_command(&mut reader, "view-end").await;

    writer
        .send(ShellWireMessage::new(
            "select",
            vec![raw("7"), raw("11"), selected_item_id],
        ))
        .await
        .unwrap();
    let acceptance = next_command(&mut reader, "accept-zsh").await;
    assert_eq!(acceptance.fields[2].as_slice(), b"portable");
    assert_eq!(acceptance.fields[4].as_slice(), b"replay-1");
    assert_eq!(acceptance.fields[5].as_slice(), b"restart");

    writer
        .send(ShellWireMessage::new("goodbye", vec![]))
        .await
        .unwrap();
    timeout(Duration::from_secs(5), bridge_task)
        .await
        .expect("bridge did not exit")
        .unwrap()
        .unwrap();
    shutdown_tx.send(()).unwrap();
    server_task.await.unwrap().unwrap();
}

#[tokio::test]
async fn late_capture_messages_after_cancellation_are_ignored() {
    let temporary = tempfile::tempdir().unwrap();
    let socket = temporary.path().join("daemon.sock");
    let server = Server::bind(ServerConfig::new(&socket)).unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let server_task = tokio::spawn(server.run_until(async {
        let _ = shutdown_rx.await;
    }));

    let (shell_to_bridge, bridge_input) = duplex(1024 * 1024);
    let (bridge_output, shell_from_bridge) = duplex(1024 * 1024);
    let bridge_task = tokio::spawn(run_bridge(
        BridgeConfig::new(&socket),
        bridge_input,
        bridge_output,
    ));
    let mut writer = FramedWrite::new(shell_to_bridge, ShellWireCodec::default());
    let mut reader = FramedRead::new(shell_from_bridge, ShellWireCodec::default());

    next_command(&mut reader, "ready").await;
    writer.send(complete()).await.unwrap();
    next_command(&mut reader, "capture-request").await;
    writer
        .send(ShellWireMessage::new("cancel", vec![raw("7"), raw("11")]))
        .await
        .unwrap();

    // A synchronous ZLE capture may already be returning when the newer edit
    // cancels it. Every late part of that capture must be harmless.
    writer
        .send(ShellWireMessage::new(
            "capture-begin",
            vec![raw("7"), raw("11"), raw("portable")],
        ))
        .await
        .unwrap();
    writer
        .send(candidate("restart", "Restart units", "replay-1", 0))
        .await
        .unwrap();
    writer
        .send(ShellWireMessage::new(
            "capture-end",
            vec![raw("7"), raw("11")],
        ))
        .await
        .unwrap();
    writer
        .send(ShellWireMessage::new("ping", vec![raw("42")]))
        .await
        .unwrap();
    let pong = next_command(&mut reader, "pong").await;
    assert_eq!(pong.fields[0].as_slice(), b"42");

    writer
        .send(ShellWireMessage::new("goodbye", vec![]))
        .await
        .unwrap();
    timeout(Duration::from_secs(5), bridge_task)
        .await
        .expect("bridge did not exit")
        .unwrap()
        .unwrap();
    shutdown_tx.send(()).unwrap();
    server_task.await.unwrap().unwrap();
}
