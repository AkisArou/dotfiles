use std::io;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

use anyhow::{Context, Result};
use futures_util::{SinkExt, StreamExt};
use nix::sys::signal::{Signal, kill};
use nix::unistd::{Pid, getuid};
use sense_protocol::{
    ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolVersion, ServerMessage,
    ShellIdentity,
};
use tokio::net::UnixStream;
use tokio_util::codec::Framed;

const PROBE_TIMEOUT: Duration = Duration::from_millis(500);
const STARTUP_POLL_INTERVAL: Duration = Duration::from_millis(10);
const STARTUP_ATTEMPTS: usize = 200;

type DaemonConnection = Framed<UnixStream, MessagePackCodec<ServerMessage, ClientMessage>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProbeResult {
    Absent,
    Ready,
    Incompatible { process_id: Pid },
}

pub async fn ensure_daemon(
    socket: &Path,
    config: Option<&Path>,
    profile: Option<&str>,
    shell: &ShellIdentity,
) -> Result<()> {
    match probe_daemon(socket, shell).await? {
        ProbeResult::Ready => return Ok(()),
        ProbeResult::Absent => {}
        ProbeResult::Incompatible { process_id } => {
            replace_incompatible_daemon(socket, process_id).await?;
        }
    }

    start_daemon(socket, config, profile, shell).await
}

async fn probe_daemon(socket: &Path, shell: &ShellIdentity) -> Result<ProbeResult> {
    let stream = match UnixStream::connect(socket).await {
        Ok(stream) => stream,
        Err(error)
            if matches!(
                error.kind(),
                io::ErrorKind::NotFound | io::ErrorKind::ConnectionRefused
            ) =>
        {
            return Ok(ProbeResult::Absent);
        }
        Err(error) => return Err(error).context("could not connect to shell-sense daemon"),
    };
    let credentials = stream
        .peer_cred()
        .context("could not authenticate shell-sense daemon peer")?;
    if credentials.uid() != getuid().as_raw() {
        anyhow::bail!("shell-sense daemon socket belongs to another user");
    }
    let process_id = credentials
        .pid()
        .map(Pid::from_raw)
        .context("the platform did not report the shell-sense daemon process ID")?;

    let mut connection = DaemonConnection::new(
        stream,
        MessagePackCodec::new(sense_protocol::DEFAULT_MAX_FRAME_BYTES),
    );
    connection
        .send(ClientMessage::Hello(ClientHello {
            protocol: ProtocolVersion::CURRENT,
            client_version: env!("CARGO_PKG_VERSION").into(),
            role: PeerRole::ShellClient,
            process_id: std::process::id(),
            shell: Some(shell.clone()),
            attach_session: None,
            attach_process_id: None,
        }))
        .await
        .context("could not probe shell-sense daemon")?;

    let response = tokio::time::timeout(PROBE_TIMEOUT, connection.next())
        .await
        .context("shell-sense daemon did not answer its protocol probe")?;
    match response {
        Some(Ok(ServerMessage::Welcome(welcome)))
            if welcome
                .protocol
                .is_compatible_with(ProtocolVersion::CURRENT) =>
        {
            connection.send(ClientMessage::Goodbye).await.ok();
            Ok(ProbeResult::Ready)
        }
        Some(Ok(ServerMessage::Error { code, message, .. }))
            if code == "handshake-failed" && message.starts_with("protocol major ") =>
        {
            Ok(ProbeResult::Incompatible { process_id })
        }
        Some(Ok(message)) => {
            anyhow::bail!("the daemon socket returned an unexpected handshake message: {message:?}")
        }
        Some(Err(error)) => Err(error).context("shell-sense daemon protocol probe failed"),
        None => anyhow::bail!("shell-sense daemon closed during its protocol probe"),
    }
}

async fn replace_incompatible_daemon(socket: &Path, process_id: Pid) -> Result<()> {
    kill(process_id, Signal::SIGTERM)
        .with_context(|| format!("could not stop incompatible shell-sense daemon {process_id}"))?;
    for _ in 0..STARTUP_ATTEMPTS {
        match UnixStream::connect(socket).await {
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::ConnectionRefused
                ) =>
            {
                return Ok(());
            }
            Err(error) => {
                return Err(error).context("could not inspect the old shell-sense daemon socket");
            }
            Ok(_) => tokio::time::sleep(STARTUP_POLL_INTERVAL).await,
        }
    }
    anyhow::bail!(
        "incompatible shell-sense daemon {process_id} did not release {} within 2 seconds",
        socket.display()
    )
}

async fn start_daemon(
    socket: &Path,
    config: Option<&Path>,
    profile: Option<&str>,
    shell: &ShellIdentity,
) -> Result<()> {
    let executable =
        std::env::current_exe().context("could not locate the shell-sense executable")?;
    let mut command = Command::new(executable);
    command
        .arg("daemon")
        .arg("--socket")
        .arg(socket)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    if let Some(config) = config {
        command.arg("--config").arg(config);
    }
    if let Some(profile) = profile {
        command.arg("--profile").arg(profile);
    }
    let mut child = command
        .spawn()
        .context("could not start shell-sense daemon")?;
    for _ in 0..STARTUP_ATTEMPTS {
        match probe_daemon(socket, shell).await? {
            ProbeResult::Ready => return Ok(()),
            ProbeResult::Absent => {}
            ProbeResult::Incompatible { process_id } => anyhow::bail!(
                "new shell-sense daemon unexpectedly reported incompatible process {process_id}"
            ),
        }
        if let Some(status) = child
            .try_wait()
            .context("could not inspect shell-sense daemon")?
        {
            anyhow::bail!("shell-sense daemon exited during startup with {status}");
        }
        tokio::time::sleep(STARTUP_POLL_INTERVAL).await;
    }
    anyhow::bail!(
        "shell-sense daemon did not become protocol-ready at {} within 2 seconds",
        socket.display()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use sense_model::{NativeShell, SessionId};
    use sense_protocol::ServerHello;
    use tokio::net::UnixListener;

    fn shell_identity() -> ShellIdentity {
        ShellIdentity {
            shell: NativeShell::Zsh,
            executable: "zsh".into(),
            version: "test".into(),
            patchlevel: None,
        }
    }

    #[tokio::test]
    async fn protocol_probe_requires_a_current_welcome() {
        let temporary = tempfile::tempdir().unwrap();
        let socket = temporary.path().join("daemon.sock");
        let listener = UnixListener::bind(&socket).unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut connection = Framed::new(
                stream,
                MessagePackCodec::<ClientMessage, ServerMessage>::new(
                    sense_protocol::DEFAULT_MAX_FRAME_BYTES,
                ),
            );
            assert!(matches!(
                connection.next().await.unwrap().unwrap(),
                ClientMessage::Hello(_)
            ));
            connection
                .send(ServerMessage::Welcome(ServerHello {
                    protocol: ProtocolVersion::CURRENT,
                    daemon_version: "test".into(),
                    session_id: SessionId::new(),
                    max_frame_bytes: u32::try_from(sense_protocol::DEFAULT_MAX_FRAME_BYTES)
                        .unwrap(),
                }))
                .await
                .unwrap();
        });

        assert_eq!(
            probe_daemon(&socket, &shell_identity()).await.unwrap(),
            ProbeResult::Ready
        );
        server.await.unwrap();
    }

    #[tokio::test]
    async fn protocol_probe_identifies_only_explicit_version_rejection() {
        let temporary = tempfile::tempdir().unwrap();
        let socket = temporary.path().join("daemon.sock");
        let listener = UnixListener::bind(&socket).unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut connection = Framed::new(
                stream,
                MessagePackCodec::<ClientMessage, ServerMessage>::new(
                    sense_protocol::DEFAULT_MAX_FRAME_BYTES,
                ),
            );
            connection.next().await.unwrap().unwrap();
            connection
                .send(ServerMessage::Error {
                    code: "handshake-failed".into(),
                    message: "protocol major 4 is incompatible with daemon major 3".into(),
                    request_id: None,
                })
                .await
                .unwrap();
        });

        assert!(matches!(
            probe_daemon(&socket, &shell_identity()).await.unwrap(),
            ProbeResult::Incompatible { .. }
        ));
        server.await.unwrap();
    }
}
