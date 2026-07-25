//! Portable Zsh probing and native-module ABI cache keys.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub const NATIVE_ABI_REVISION: u16 = 1;

const PROBE_SCRIPT: &str = r#"
print -r -- "${commands[zsh]:-$0}"
print -r -- "$ZSH_VERSION"
print -r -- "$ZSH_PATCHLEVEL"
setopt localoptions nullglob
typeset -a sense_modules
sense_modules=($^module_path/zsh/complete.*(N))
print -r -- "${sense_modules[1]:e}"
if zmodload zsh/complete 2>/dev/null; then print yes; else print no; fi
"#;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TargetIdentity {
    pub triple: String,
    pub pointer_width: String,
    pub endian: String,
}

impl TargetIdentity {
    #[must_use]
    pub fn build_target() -> Self {
        Self {
            triple: env!("SENSE_BUILD_TARGET").into(),
            pointer_width: env!("SENSE_BUILD_POINTER_WIDTH").into(),
            endian: env!("SENSE_BUILD_ENDIAN").into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ZshIdentity {
    pub executable: PathBuf,
    pub version: String,
    pub patchlevel: String,
    pub module_suffix: String,
    pub dynamic_modules: bool,
    pub executable_digest: String,
    pub target: TargetIdentity,
}

impl ZshIdentity {
    #[must_use]
    pub fn abi_key(&self) -> String {
        compute_abi_key(self)
    }
}

#[derive(Debug, Error)]
pub enum ProbeError {
    #[error("could not execute or inspect Zsh: {0}")]
    Io(#[from] io::Error),
    #[error("Zsh probe exited with {status}: {stderr}")]
    Failed { status: String, stderr: String },
    #[error("Zsh probe output is invalid: {0}")]
    InvalidOutput(String),
}

/// Probe a concrete Zsh executable for the build target of this binary.
///
/// # Errors
///
/// Returns an error when the executable cannot be run/read, exits
/// unsuccessfully, or returns an incomplete identity.
pub fn probe(executable: impl AsRef<Path>) -> Result<ZshIdentity, ProbeError> {
    probe_for_target(executable, TargetIdentity::build_target())
}

/// Probe a concrete Zsh executable for an explicitly selected target.
///
/// This variant is used by the native-module build script so cross builds do
/// not accidentally use the build-script host triple in the ABI key.
///
/// # Errors
///
/// Returns an error when the executable cannot be run/read, exits
/// unsuccessfully, or returns an incomplete identity.
pub fn probe_for_target(
    executable: impl AsRef<Path>,
    target: TargetIdentity,
) -> Result<ZshIdentity, ProbeError> {
    let output = Command::new(executable.as_ref())
        .args(["-f", "-c", PROBE_SCRIPT])
        .output()?;
    if !output.status.success() {
        return Err(ProbeError::Failed {
            status: output.status.to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).trim().to_owned(),
        });
    }
    let stdout = String::from_utf8(output.stdout)
        .map_err(|error| ProbeError::InvalidOutput(error.to_string()))?;
    let mut lines = stdout.lines();
    let reported = required_line(&mut lines, "executable")?;
    let version = required_line(&mut lines, "version")?;
    let patchlevel = required_line(&mut lines, "patchlevel")?;
    let module_suffix = lines
        .next()
        .map(str::trim)
        .filter(|suffix| !suffix.is_empty())
        .unwrap_or("so")
        .to_owned();
    let dynamic_modules = lines.next().is_some_and(|line| line.trim() == "yes");
    let executable = fs::canonicalize(&reported).unwrap_or_else(|_| PathBuf::from(reported));
    let executable_digest = blake3::hash(&fs::read(&executable)?).to_hex().to_string();
    Ok(ZshIdentity {
        executable,
        version,
        patchlevel,
        module_suffix,
        dynamic_modules,
        executable_digest,
        target,
    })
}

fn required_line<'a>(
    lines: &mut impl Iterator<Item = &'a str>,
    field: &str,
) -> Result<String, ProbeError> {
    lines
        .next()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_owned)
        .ok_or_else(|| ProbeError::InvalidOutput(format!("missing {field}")))
}

#[must_use]
pub fn compute_abi_key(identity: &ZshIdentity) -> String {
    let revision = NATIVE_ABI_REVISION.to_string();
    let mut hasher = blake3::Hasher::new();
    for component in [
        identity.target.triple.as_str(),
        identity.target.pointer_width.as_str(),
        identity.target.endian.as_str(),
        identity.version.as_str(),
        identity.patchlevel.as_str(),
        identity.module_suffix.as_str(),
        identity.executable_digest.as_str(),
        revision.as_str(),
    ] {
        hasher.update(component.as_bytes());
        hasher.update(&[0]);
    }
    hasher.finalize().to_hex().to_string()
}
