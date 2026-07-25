//! Manifest and wire contracts for crash-isolated third-party adapters.
//!
//! This crate contains data only. External adapters are ordinary processes;
//! they are never loaded as dynamic libraries into Zsh or the daemon.

use std::collections::BTreeMap;

use semver::Version;
use sense_model::{CompletionItem, Diagnostic, Generation, ItemId, RequestId, SessionId};
use sense_provider_api::{ProviderContext, ProviderDescriptor, ProviderError, ProviderEvent};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;

pub const ADAPTER_MANIFEST_VERSION: u16 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdapterProtocolVersion {
    pub major: u16,
    pub minor: u16,
}

impl AdapterProtocolVersion {
    pub const CURRENT: Self = Self { major: 1, minor: 0 };

    #[must_use]
    pub const fn is_compatible_with(self, other: Self) -> bool {
        self.major == other.major
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AdapterOrigin {
    Installed,
    User,
    Project,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SideEffectDeclaration {
    None,
    ReadOnlyCommands,
    MutatingCommands,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdapterPermissions {
    pub environment_allowlist: Vec<String>,
    pub network: bool,
    pub side_effects: SideEffectDeclaration,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AdapterManifest {
    pub manifest_version: u16,
    pub protocol: AdapterProtocolVersion,
    pub provider: ProviderDescriptor,
    /// Executable followed by fixed arguments. No shell is involved.
    pub command: Vec<String>,
    pub origin: AdapterOrigin,
    pub permissions: AdapterPermissions,
    pub configuration_schema: Option<Value>,
    pub default_configuration: BTreeMap<String, Value>,
    pub maximum_output_bytes: u64,
}

impl AdapterManifest {
    /// Validate the manifest before an adapter process is started.
    ///
    /// # Errors
    ///
    /// Returns an error for incompatible versions, invalid provider
    /// declarations, an empty command, or an unbounded output declaration.
    pub fn validate(&self) -> Result<(), ManifestError> {
        if self.manifest_version != ADAPTER_MANIFEST_VERSION {
            return Err(ManifestError::UnsupportedManifestVersion {
                expected: ADAPTER_MANIFEST_VERSION,
                actual: self.manifest_version,
            });
        }
        if !self
            .protocol
            .is_compatible_with(AdapterProtocolVersion::CURRENT)
        {
            return Err(ManifestError::UnsupportedProtocolVersion {
                expected: AdapterProtocolVersion::CURRENT.major,
                actual: self.protocol.major,
            });
        }
        self.provider.validate()?;
        if self.command.first().is_none_or(String::is_empty) {
            return Err(ManifestError::EmptyCommand);
        }
        if self.maximum_output_bytes == 0 {
            return Err(ManifestError::InvalidOutputLimit);
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("manifest version {actual} is unsupported; expected {expected}")]
    UnsupportedManifestVersion { expected: u16, actual: u16 },
    #[error("adapter protocol major {actual} is unsupported; expected {expected}")]
    UnsupportedProtocolVersion { expected: u16, actual: u16 },
    #[error("adapter command must include a non-empty executable")]
    EmptyCommand,
    #[error("maximum_output_bytes must be greater than zero")]
    InvalidOutputLimit,
    #[error(transparent)]
    Provider(#[from] ProviderError),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum AdapterOperation {
    Complete,
    Enrich { items: Vec<CompletionItem> },
    Resolve { item: CompletionItem },
    Signature,
    Diagnose,
    Actions { diagnostics: Vec<Diagnostic> },
    Preview { item: CompletionItem },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AdapterRequest {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    pub context: ProviderContext,
    pub operation: AdapterOperation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum DaemonToAdapter {
    Hello {
        protocol: AdapterProtocolVersion,
        daemon_version: Version,
        configuration: BTreeMap<String, Value>,
    },
    Request(Box<AdapterRequest>),
    Cancel {
        session_id: SessionId,
        request_id: RequestId,
        generation: Generation,
    },
    Ping {
        nonce: u64,
    },
    Shutdown,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum AdapterToDaemon {
    Ready {
        protocol: AdapterProtocolVersion,
        adapter_id: String,
        adapter_version: Version,
    },
    Event {
        session_id: SessionId,
        request_id: RequestId,
        generation: Generation,
        event: ProviderEvent,
    },
    Finished {
        session_id: SessionId,
        request_id: RequestId,
        generation: Generation,
        cancelled: bool,
    },
    SelectionResolved {
        session_id: SessionId,
        request_id: RequestId,
        generation: Generation,
        item_id: ItemId,
    },
    Pong {
        nonce: u64,
    },
    Error {
        request_id: Option<RequestId>,
        code: String,
        message: String,
    },
}
