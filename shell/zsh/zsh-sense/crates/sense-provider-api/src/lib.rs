//! Stable in-process API for completion sources and semantic context adapters.
//!
//! Providers never render, edit ZLE state, or block the shell. They publish
//! bounded events to the daemon and cooperate with cancellation.

use std::collections::BTreeMap;
use std::time::Duration;

use async_trait::async_trait;
use bitflags::bitflags;
use semver::Version;
use sense_model::{
    CodeAction, CompletionItem, CompletionKind, CompletionRequest, Diagnostic, DocumentationState,
    ItemId, Preview, RawBytes, SignatureHelp, SourceId, TextRange,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

pub const PROVIDER_API_VERSION: u16 = 1;

bitflags! {
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct ProviderCapabilities: u16 {
        const COMPLETE = 1 << 0;
        const ENRICH = 1 << 1;
        const RESOLVE = 1 << 2;
        const SIGNATURE = 1 << 3;
        const DIAGNOSE = 1 << 4;
        const ACTIONS = 1 << 5;
        const PREVIEW = 1 << 6;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ProviderClass {
    Source,
    ContextAdapter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Authority {
    Advisory,
    Inferred,
    Partial,
    Authoritative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct DeadlinePolicy {
    pub soft_ms: u64,
    pub hard_ms: u64,
}

impl DeadlinePolicy {
    #[must_use]
    pub const fn soft(self) -> Duration {
        Duration::from_millis(self.soft_ms)
    }

    #[must_use]
    pub const fn hard(self) -> Duration {
        Duration::from_millis(self.hard_ms)
    }

    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.soft_ms <= self.hard_ms && self.hard_ms > 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProviderSelector {
    /// Command paths, such as `["git", "checkout"]`. Empty matches all.
    pub command_paths: Vec<Vec<String>>,
    /// Optional semantic contexts, such as `option` or `path`.
    pub contexts: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProviderDescriptor {
    pub id: SourceId,
    pub display_name: String,
    pub version: Version,
    pub api_version: u16,
    pub class: ProviderClass,
    pub capabilities: ProviderCapabilities,
    pub authority: Authority,
    pub selectors: ProviderSelector,
    pub deadlines: DeadlinePolicy,
    pub maximum_concurrency: u16,
    pub maximum_candidates: u32,
    pub cancellation: bool,
    pub side_effect_free: bool,
}

impl ProviderDescriptor {
    /// Validate compatibility and resource bounds.
    ///
    /// # Errors
    ///
    /// Returns [`ProviderError`] when the descriptor uses an unsupported API
    /// version or declares invalid deadline/concurrency limits.
    pub fn validate(&self) -> Result<(), ProviderError> {
        if self.api_version != PROVIDER_API_VERSION {
            return Err(ProviderError::UnsupportedApiVersion {
                provider: self.id.0.clone(),
                expected: PROVIDER_API_VERSION,
                actual: self.api_version,
            });
        }
        if !self.deadlines.is_valid() {
            return Err(ProviderError::InvalidDescriptor(format!(
                "provider {} has invalid deadlines",
                self.id.0
            )));
        }
        if self.maximum_concurrency == 0 {
            return Err(ProviderError::InvalidDescriptor(format!(
                "provider {} must allow at least one request",
                self.id.0
            )));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProviderContext {
    pub request: CompletionRequest,
    pub command_path: Vec<RawBytes>,
    pub current_token: Option<TextRange>,
    pub expected_kinds: Vec<CompletionKind>,
    pub project_root: Option<RawBytes>,
    /// Sanitized parser/project facts. This is not the process environment.
    pub facts: BTreeMap<String, RawBytes>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enrichment {
    pub item_id: ItemId,
    pub kind: Option<CompletionKind>,
    pub detail: Option<String>,
    pub documentation: Option<DocumentationState>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum ProviderEvent {
    Candidates {
        items: Vec<CompletionItem>,
        is_incomplete: bool,
    },
    Enrichments(Vec<Enrichment>),
    Documentation {
        item_id: ItemId,
        documentation: DocumentationState,
    },
    Signature(SignatureHelp),
    Diagnostics(Vec<Diagnostic>),
    Actions(Vec<CodeAction>),
    Preview(Preview),
    Status(String),
}

#[derive(Debug, Error)]
pub enum ProviderError {
    #[error("provider request was cancelled")]
    Cancelled,
    #[error("provider output channel was closed")]
    OutputClosed,
    #[error("provider {provider} uses API {actual}; expected {expected}")]
    UnsupportedApiVersion {
        provider: String,
        expected: u16,
        actual: u16,
    },
    #[error("invalid provider descriptor: {0}")]
    InvalidDescriptor(String),
    #[error("provider failed: {0}")]
    Failed(String),
}

#[derive(Debug, Clone)]
pub struct ProviderSink {
    sender: mpsc::Sender<ProviderEvent>,
}

impl ProviderSink {
    #[must_use]
    pub const fn new(sender: mpsc::Sender<ProviderEvent>) -> Self {
        Self { sender }
    }

    /// Publish an event while remaining responsive to cancellation.
    ///
    /// # Errors
    ///
    /// Returns [`ProviderError::Cancelled`] when the request is cancelled, or
    /// [`ProviderError::OutputClosed`] when the daemon stopped receiving.
    pub async fn send(
        &self,
        event: ProviderEvent,
        cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        tokio::select! {
            () = cancellation.cancelled() => Err(ProviderError::Cancelled),
            result = self.sender.send(event) => result.map_err(|_| ProviderError::OutputClosed),
        }
    }
}

#[async_trait]
pub trait Provider: Send + Sync + 'static {
    fn descriptor(&self) -> &ProviderDescriptor;

    async fn complete(
        &self,
        _context: &ProviderContext,
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn enrich(
        &self,
        _context: &ProviderContext,
        _items: &[CompletionItem],
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn resolve(
        &self,
        _context: &ProviderContext,
        _item: &CompletionItem,
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn signature(
        &self,
        _context: &ProviderContext,
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn diagnose(
        &self,
        _context: &ProviderContext,
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn actions(
        &self,
        _context: &ProviderContext,
        _diagnostics: &[Diagnostic],
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn preview(
        &self,
        _context: &ProviderContext,
        _item: &CompletionItem,
        _sink: &ProviderSink,
        _cancellation: &CancellationToken,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
}
