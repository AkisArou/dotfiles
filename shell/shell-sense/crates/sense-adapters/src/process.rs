use std::ffi::{OsStr, OsString};
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::process::Stdio;
use std::time::Duration;

use sense_model::RawBytes;
use sense_provider_api::ProviderError;
use tokio::io::{AsyncRead, AsyncReadExt};
use tokio::process::Command;
use tokio_util::sync::CancellationToken;

pub struct CommandRequest {
    pub program: RawBytes,
    pub arguments: Vec<RawBytes>,
    pub cwd: RawBytes,
    pub timeout: Duration,
    pub maximum_output_bytes: usize,
}

pub async fn run_bounded(
    request: CommandRequest,
    cancellation: &CancellationToken,
) -> Result<String, ProviderError> {
    let program_name = request.program.display_lossy();
    let mut command = Command::new(raw_os_string(&request.program));
    command
        .args(request.arguments.iter().map(raw_os_string))
        .current_dir(raw_os_string(&request.cwd))
        .env("LC_ALL", "C")
        .env("TERM", "dumb")
        .env("NO_COLOR", "1")
        .env("GIT_PAGER", "cat")
        .env("MANWIDTH", "100")
        .env("SYSTEMD_PAGER", "cat")
        .kill_on_drop(true)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = command
        .spawn()
        .map_err(|error| ProviderError::Failed(error.to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| ProviderError::Failed("documentation stdout was not captured".into()))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| ProviderError::Failed("documentation stderr was not captured".into()))?;

    let outcome = {
        let operation = async {
            let (status, stdout, stderr) = tokio::try_join!(
                child.wait(),
                read_bounded(stdout, request.maximum_output_bytes),
                read_bounded(stderr, request.maximum_output_bytes),
            )?;
            Ok::<_, io::Error>((status, stdout, stderr))
        };
        tokio::pin!(operation);
        tokio::select! {
            () = cancellation.cancelled() => CommandOutcome::Cancelled,
            () = tokio::time::sleep(request.timeout) => CommandOutcome::TimedOut,
            result = &mut operation => CommandOutcome::Finished(result),
        }
    };

    let (status, stdout, stderr) = match outcome {
        CommandOutcome::Cancelled => {
            terminate(&mut child).await;
            return Err(ProviderError::Cancelled);
        }
        CommandOutcome::TimedOut => {
            terminate(&mut child).await;
            return Err(ProviderError::Failed(format!(
                "{program_name} documentation timed out"
            )));
        }
        CommandOutcome::Finished(result) => {
            result.map_err(|error| ProviderError::Failed(error.to_string()))?
        }
    };
    let output = select_output(stdout, stderr, request.maximum_output_bytes);
    if !status.success() && output.is_empty() {
        return Err(ProviderError::Failed(format!(
            "{program_name} documentation exited with {status}"
        )));
    }
    Ok(remove_overstrikes(&String::from_utf8_lossy(&output)))
}

enum CommandOutcome {
    Cancelled,
    TimedOut,
    Finished(io::Result<(std::process::ExitStatus, Vec<u8>, Vec<u8>)>),
}

async fn terminate(child: &mut tokio::process::Child) {
    if let Err(error) = child.kill().await
        && error.kind() != io::ErrorKind::InvalidInput
    {
        tracing::debug!(%error, "could not terminate documentation command");
    }
}

async fn read_bounded(
    mut reader: impl AsyncRead + Unpin,
    maximum_bytes: usize,
) -> io::Result<Vec<u8>> {
    let mut output = Vec::with_capacity(maximum_bytes.min(8192));
    let mut buffer = Box::new([0_u8; 8192]);
    loop {
        let read = reader.read(&mut buffer[..]).await?;
        if read == 0 {
            break;
        }
        let remaining = maximum_bytes.saturating_sub(output.len());
        output.extend_from_slice(&buffer[..read.min(remaining)]);
    }
    Ok(output)
}

fn select_output(mut stdout: Vec<u8>, stderr: Vec<u8>, maximum_bytes: usize) -> Vec<u8> {
    if stdout.is_empty() {
        return stderr;
    }
    if !stderr.is_empty() && stdout.len() < maximum_bytes {
        stdout.push(b'\n');
        let remaining = maximum_bytes.saturating_sub(stdout.len());
        stdout.extend_from_slice(&stderr[..stderr.len().min(remaining)]);
    }
    stdout
}

fn raw_os_string(bytes: &RawBytes) -> OsString {
    OsStr::from_bytes(bytes.as_slice()).to_os_string()
}

fn remove_overstrikes(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    for character in input.chars() {
        if character == '\u{8}' {
            output.pop();
        } else if character != '\r' {
            output.push(character);
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::remove_overstrikes;

    #[test]
    fn overstrike_formatting_becomes_plain_text() {
        assert_eq!(
            remove_overstrikes("N\u{8}NA\u{8}AM\u{8}ME\u{8}E\r\n"),
            "NAME\n"
        );
        assert_eq!(remove_overstrikes("_\u{8}o_\u{8}p_\u{8}t"), "opt");
    }
}
