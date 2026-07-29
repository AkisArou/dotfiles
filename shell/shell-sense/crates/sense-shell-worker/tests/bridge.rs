use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use sense_daemon::{Server, ServerConfig};
use sense_model::{NativeShell, RawBytes};
use sense_protocol::ShellIdentity;
use sense_shell_worker::{BridgeConfig, ShellWireCodec, ShellWireMessage, run_bridge};
use tokio::io::duplex;
use tokio::sync::oneshot;
use tokio::time::timeout;
use tokio_util::codec::{FramedRead, FramedWrite};

fn raw(value: impl Into<RawBytes>) -> RawBytes {
    value.into()
}

fn bridge_config(socket: &std::path::Path) -> BridgeConfig {
    bridge_config_for(socket, NativeShell::Zsh)
}

fn bridge_config_for(socket: &std::path::Path, shell: NativeShell) -> BridgeConfig {
    BridgeConfig::new(
        socket,
        ShellIdentity {
            shell,
            executable: format!("/bin/{}", shell.source_name()),
            version: "test".into(),
            patchlevel: None,
        },
    )
}

fn shell_candidate(label: &str, description: &str, order: u32) -> ShellWireMessage {
    ShellWireMessage::new(
        "shell-candidate",
        vec![
            raw("7"),
            raw("11"),
            raw(label),
            raw(label),
            raw(description),
            raw("subcommands"),
            raw("10"),
            raw("16"),
            raw("subcommand"),
            raw(order.to_string()),
            raw("1"),
            raw("0"),
            raw(format!("fish-{order}")),
            RawBytes::default(),
        ],
    )
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
        "zsh-candidate",
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

fn assert_ranked_zsh_view(view_begin: &ShellWireMessage, first: &ShellWireMessage) -> RawBytes {
    assert_eq!(view_begin.fields[4].as_slice(), b"0");
    assert_eq!(view_begin.fields[8].as_slice(), b"1");
    assert_eq!(first.fields[2].as_slice(), b"1");
    assert_eq!(first.fields[4].as_slice(), b"restart");
    assert_eq!(first.fields[5].as_slice(), b"7");
    assert_eq!(first.fields[7].as_slice(), "󰆍".as_bytes());
    assert_eq!(first.fields[8].as_slice(), b"Restart one or more units");
    assert_eq!(first.fields[11].as_slice(), b"zsh");
    assert_eq!(first.fields[12].as_slice(), b"replay-1");
    assert!(first.fields[14].is_empty());
    first.fields[3].clone()
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
        bridge_config(&socket),
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
            "zsh-capture-begin",
            vec![raw("7"), raw("11")],
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
    let first = next_command(&mut reader, "view-chunk").await;
    let selected_item_id = assert_ranked_zsh_view(&view_begin, &first);
    let layout = next_command(&mut reader, "view-layout").await;
    assert_eq!(layout.fields[0].as_slice(), b"7");
    next_command(&mut reader, "view-end").await;

    writer
        .send(ShellWireMessage::new(
            "navigate",
            vec![raw("7"), raw("11"), raw("1"), raw("next")],
        ))
        .await
        .unwrap();
    let selection = next_command(&mut reader, "selection-changed").await;
    assert_eq!(selection.fields[3].as_slice(), b"1");
    let navigation_applied = next_command(&mut reader, "navigation-applied").await;
    assert_eq!(
        navigation_applied.fields,
        vec![raw("7"), raw("11"), raw("1")]
    );

    writer
        .send(ShellWireMessage::new(
            "select",
            vec![raw("7"), raw("11"), selected_item_id.clone()],
        ))
        .await
        .unwrap();
    let acceptance = next_command(&mut reader, "accept-zsh").await;
    assert_eq!(acceptance.fields[2], selected_item_id);
    assert_eq!(acceptance.fields[3].as_slice(), b"1");
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
async fn fish_native_candidates_use_the_same_rank_and_selection_pipeline() {
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
        bridge_config_for(&socket, NativeShell::Fish),
        bridge_input,
        bridge_output,
    ));
    let mut writer = FramedWrite::new(shell_to_bridge, ShellWireCodec::default());
    let mut reader = FramedRead::new(shell_from_bridge, ShellWireCodec::default());

    next_command(&mut reader, "ready").await;
    writer.send(complete()).await.unwrap();
    next_command(&mut reader, "capture-request").await;
    writer
        .send(ShellWireMessage::new(
            "shell-capture-begin",
            vec![raw("7"), raw("11")],
        ))
        .await
        .unwrap();
    writer
        .send(shell_candidate("reset-failed", "Reset failed state", 0))
        .await
        .unwrap();
    writer
        .send(shell_candidate("restart", "Restart services", 1))
        .await
        .unwrap();
    writer
        .send(ShellWireMessage::new(
            "capture-end",
            vec![raw("7"), raw("11")],
        ))
        .await
        .unwrap();

    next_command(&mut reader, "view-begin").await;
    let view = next_command(&mut reader, "view-chunk").await;
    assert_eq!(view.fields[4].as_slice(), b"restart");
    assert_eq!(view.fields[11].as_slice(), b"fish");
    let item_id = view.fields[3].clone();
    next_command(&mut reader, "view-end").await;

    writer
        .send(ShellWireMessage::new(
            "select",
            vec![raw("7"), raw("11"), item_id.clone()],
        ))
        .await
        .unwrap();
    let acceptance = next_command(&mut reader, "accept-fish").await;
    assert_eq!(acceptance.fields[2], item_id);
    assert_eq!(acceptance.fields[3].as_slice(), b"restart");
    assert_eq!(acceptance.fields[4].as_slice(), b"10");
    assert_eq!(acceptance.fields[5].as_slice(), b"16");
    assert_eq!(acceptance.fields[6].as_slice(), b"1");

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
        bridge_config(&socket),
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
            "zsh-capture-begin",
            vec![raw("7"), raw("11")],
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
