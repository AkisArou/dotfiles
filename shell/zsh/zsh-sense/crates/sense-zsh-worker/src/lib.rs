//! Request-scoped state shared by native and portable Zsh capture backends.
//!
//! Candidate labels are untrusted display data. Acceptance always resolves an
//! opaque fingerprint back to the live worker record that created it.

mod bridge;
mod shell_wire;

pub use bridge::*;
pub use shell_wire::*;

use std::collections::HashMap;

use bitflags::bitflags;
use sense_model::{
    CompletionItem, CompletionKind, Confidence, DocumentationState, Generation, GroupId,
    InsertStrategy, ItemCapabilities, ItemId, ItemTags, RawBytes, RequestId, SourceId, TextEdit,
    TextRange,
};
use sense_protocol::CandidateBatch;
use serde::{Deserialize, Serialize};
use thiserror::Error;

const CAPTURE_FINGERPRINT_DOMAIN: &[u8] = b"zsh-sense/capture-fingerprint/v1";
const ITEM_ID_DOMAIN: &[u8] = b"zsh-sense/stable-zsh-item/v1";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CaptureBackend {
    Native,
    Portable,
}

bitflags! {
    /// Backend-neutral subset of Zsh match behavior required for presentation
    /// and acceptance diagnostics. The backend retains its complete opaque
    /// record in addition to these flags.
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct ZshMatchFlags: u32 {
        const FILE = 1 << 0;
        const DIRECTORY = 1 << 1;
        const SYMLINK = 1 << 2;
        const NO_SPACE = 1 << 3;
        const REMOVE_SUFFIX = 1 << 4;
        const ALREADY_QUOTED = 1 << 5;
        const HIDDEN = 1 << 6;
        const NO_LIST = 1 << 7;
        const DISPLAY_LINE = 1 << 8;
        const PARAMETER = 1 << 9;
        const DUMMY = 1 << 10;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapturedGroup {
    pub name: String,
    pub description: Option<String>,
    pub order: u32,
}

/// The insertion components represented by `compadd` and the live completion
/// state. Raw bytes are retained even when they cannot be displayed as UTF-8.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ZshInsertionMetadata {
    pub prefix: RawBytes,
    pub suffix: RawBytes,
    pub hidden_prefix: RawBytes,
    pub hidden_suffix: RawBytes,
    pub ignored_prefix: RawBytes,
    pub ignored_suffix: RawBytes,
    pub path_prefix: RawBytes,
    pub path_suffix: RawBytes,
    pub path_directory: RawBytes,
    pub removable_suffix_characters: RawBytes,
    pub suffix_removal_function: RawBytes,
    pub matcher_specs: Vec<RawBytes>,
}

/// One candidate captured from the real Zsh completion engine.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapturedMatch {
    /// Raw completion word. This is never reconstructed from `display`.
    pub insertion: RawBytes,
    pub display: Option<String>,
    pub description: Option<String>,
    pub explanation: Option<String>,
    pub group: Option<CapturedGroup>,
    pub replace_range: TextRange,
    pub kind: CompletionKind,
    pub flags: ZshMatchFlags,
    pub insertion_metadata: ZshInsertionMetadata,
    /// Native match identity or portable replay token, meaningful only to the
    /// backend that produced this request.
    pub backend_identity: RawBytes,
    pub original_order: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CaptureLimits {
    pub max_candidates: usize,
    pub max_bytes: usize,
    pub max_ui_bytes_per_field: usize,
}

impl Default for CaptureLimits {
    fn default() -> Self {
        Self {
            max_candidates: 100_000,
            max_bytes: 64 * 1024 * 1024,
            max_ui_bytes_per_field: 16 * 1024,
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CaptureError {
    #[error("capture limits must all be greater than zero")]
    InvalidLimits,
    #[error("candidate {index} replacement range is outside the command buffer")]
    InvalidRange { index: usize },
    #[error("no active capture matches request {request_id:?} generation {generation:?}")]
    StaleCapture {
        request_id: RequestId,
        generation: Generation,
    },
    #[error("the selected Zsh fingerprint is not present in the active capture")]
    UnknownFingerprint,
}

/// Data required by the selected backend to replay Zsh acceptance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AcceptanceRoute {
    pub backend: CaptureBackend,
    pub request_id: RequestId,
    pub generation: Generation,
    pub ordinal: u32,
    pub insertion: RawBytes,
    pub flags: ZshMatchFlags,
    pub insertion_metadata: ZshInsertionMetadata,
    pub backend_identity: RawBytes,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaptureOutcome {
    pub batch: CandidateBatch,
    pub captured: usize,
    pub dropped: usize,
    pub captured_bytes: usize,
}

#[derive(Debug)]
struct ActiveCapture {
    request_id: RequestId,
    generation: Generation,
    routes: HashMap<RawBytes, AcceptanceRoute>,
    item_fingerprints: HashMap<ItemId, RawBytes>,
}

/// A worker intentionally retains only the newest capture generation. This
/// makes stale acceptance impossible after another structural completion.
#[derive(Debug)]
pub struct CaptureStore {
    limits: CaptureLimits,
    active: Option<ActiveCapture>,
}

impl CaptureStore {
    /// Create a bounded request store.
    ///
    /// # Errors
    ///
    /// Returns [`CaptureError::InvalidLimits`] if any limit is zero.
    pub fn new(limits: CaptureLimits) -> Result<Self, CaptureError> {
        if limits.max_candidates == 0 || limits.max_bytes == 0 || limits.max_ui_bytes_per_field == 0
        {
            return Err(CaptureError::InvalidLimits);
        }
        Ok(Self {
            limits,
            active: None,
        })
    }

    #[must_use]
    pub const fn limits(&self) -> CaptureLimits {
        self.limits
    }

    /// Replace the active generation and create a final Zsh candidate batch.
    ///
    /// Candidates beyond the configured count/byte bounds are dropped as a
    /// tail and the batch is marked incomplete. Invalid edit ranges fail the
    /// entire capture because accepting them could corrupt the command line.
    ///
    /// # Errors
    ///
    /// Returns an invalid-range error when any supplied replacement range is
    /// outside the request buffer.
    pub fn install(
        &mut self,
        request: &sense_model::CompletionRequest,
        backend: CaptureBackend,
        matches: Vec<CapturedMatch>,
    ) -> Result<CaptureOutcome, CaptureError> {
        for (index, captured) in matches.iter().enumerate() {
            if !captured
                .replace_range
                .is_valid_for(request.buffer.as_slice())
            {
                return Err(CaptureError::InvalidRange { index });
            }
        }

        let total = matches.len();
        let mut bytes = 0_usize;
        let mut items = Vec::new();
        let mut routes = HashMap::new();
        let mut item_fingerprints = HashMap::new();

        for (index, captured) in matches.into_iter().enumerate() {
            let candidate_bytes = capture_size(&captured);
            if items.len() == self.limits.max_candidates
                || bytes.saturating_add(candidate_bytes) > self.limits.max_bytes
            {
                break;
            }
            bytes += candidate_bytes;
            let ordinal = u32::try_from(index).unwrap_or(u32::MAX);
            let fingerprint = capture_fingerprint(request, backend, ordinal, &captured);
            let item = completion_item(&captured, &fingerprint, self.limits.max_ui_bytes_per_field);
            item_fingerprints.insert(item.id.clone(), fingerprint.clone());
            routes.insert(
                fingerprint.clone(),
                AcceptanceRoute {
                    backend,
                    request_id: request.request_id,
                    generation: request.generation,
                    ordinal,
                    insertion: captured.insertion,
                    flags: captured.flags,
                    insertion_metadata: captured.insertion_metadata,
                    backend_identity: captured.backend_identity,
                },
            );
            items.push(item);
        }

        let captured = items.len();
        let dropped = total.saturating_sub(captured);
        self.active = Some(ActiveCapture {
            request_id: request.request_id,
            generation: request.generation,
            routes,
            item_fingerprints,
        });
        Ok(CaptureOutcome {
            batch: CandidateBatch {
                session_id: request.session_id,
                request_id: request.request_id,
                generation: request.generation,
                source: SourceId("zsh".into()),
                items,
                is_final: true,
                is_incomplete: dropped != 0,
            },
            captured,
            dropped,
            captured_bytes: bytes,
        })
    }

    /// Resolve a daemon selection to the live backend record.
    ///
    /// # Errors
    ///
    /// Rejects stale generations and fingerprints not issued by the active
    /// generation.
    pub fn acceptance(
        &self,
        request_id: RequestId,
        generation: Generation,
        fingerprint: &RawBytes,
    ) -> Result<&AcceptanceRoute, CaptureError> {
        let Some(active) = &self.active else {
            return Err(CaptureError::StaleCapture {
                request_id,
                generation,
            });
        };
        if active.request_id != request_id || active.generation != generation {
            return Err(CaptureError::StaleCapture {
                request_id,
                generation,
            });
        }
        active
            .routes
            .get(fingerprint)
            .ok_or(CaptureError::UnknownFingerprint)
    }

    /// Resolve a daemon-visible stable item identifier to its live Zsh match.
    ///
    /// # Errors
    ///
    /// Rejects stale generations and item identifiers not issued by the
    /// active capture.
    pub fn acceptance_by_item(
        &self,
        request_id: RequestId,
        generation: Generation,
        item_id: &ItemId,
    ) -> Result<&AcceptanceRoute, CaptureError> {
        let Some(active) = &self.active else {
            return Err(CaptureError::StaleCapture {
                request_id,
                generation,
            });
        };
        if active.request_id != request_id || active.generation != generation {
            return Err(CaptureError::StaleCapture {
                request_id,
                generation,
            });
        }
        let fingerprint = active
            .item_fingerprints
            .get(item_id)
            .ok_or(CaptureError::UnknownFingerprint)?;
        active
            .routes
            .get(fingerprint)
            .ok_or(CaptureError::UnknownFingerprint)
    }

    pub fn cancel(&mut self, request_id: RequestId, generation: Generation) -> bool {
        if self.active.as_ref().is_some_and(|active| {
            active.request_id == request_id && active.generation == generation
        }) {
            self.active = None;
            true
        } else {
            false
        }
    }
}

fn completion_item(
    captured: &CapturedMatch,
    fingerprint: &RawBytes,
    max_ui_bytes: usize,
) -> CompletionItem {
    let fallback_label = captured.insertion.display_lossy();
    let label = sanitize_ui(
        captured.display.as_deref().unwrap_or(&fallback_label),
        max_ui_bytes,
    );
    let detail = captured
        .description
        .as_deref()
        .or(captured.explanation.as_deref())
        .map(|text| sanitize_ui(text, max_ui_bytes));
    let group = captured.group.as_ref().map(|group| {
        GroupId(format!(
            "zsh:{}:{}",
            group.order,
            sanitize_ui(&group.name, max_ui_bytes)
        ))
    });
    let mut tags = ItemTags::empty();
    if captured.flags.contains(ZshMatchFlags::HIDDEN) {
        tags |= ItemTags::HIDDEN;
    }
    CompletionItem {
        id: ItemId(stable_item_id(captured)),
        source: SourceId("zsh".into()),
        label,
        label_detail: captured
            .group
            .as_ref()
            .and_then(|group| group.description.as_deref())
            .map(|text| sanitize_ui(text, max_ui_bytes)),
        filter_text: Some(sanitize_ui(&fallback_label, max_ui_bytes)),
        sort_text: None,
        kind: captured.kind,
        tags,
        detail,
        documentation: DocumentationState::None,
        group,
        edit: TextEdit::new(captured.replace_range, captured.insertion.clone()),
        insertion: InsertStrategy::ZshMatch {
            fingerprint: fingerprint.clone(),
        },
        commit_characters: Vec::new(),
        original_order: captured.original_order,
        provider_relevance: 0,
        confidence: Confidence::Authoritative,
        capabilities: ItemCapabilities::PARTIAL_ACCEPT,
        match_result: None,
        opaque_data: RawBytes::default(),
    }
}

fn capture_fingerprint(
    request: &sense_model::CompletionRequest,
    backend: CaptureBackend,
    ordinal: u32,
    captured: &CapturedMatch,
) -> RawBytes {
    let mut hash = blake3::Hasher::new();
    hash.update(CAPTURE_FINGERPRINT_DOMAIN);
    hash.update(request.session_id.0.as_bytes());
    hash.update(&request.request_id.0.to_le_bytes());
    hash.update(&request.generation.0.to_le_bytes());
    hash.update(&[backend_tag(backend)]);
    hash.update(&ordinal.to_le_bytes());
    hash_bytes(&mut hash, captured.backend_identity.as_slice());
    hash_bytes(&mut hash, captured.insertion.as_slice());
    RawBytes::from(hash.finalize().as_bytes().as_slice())
}

fn stable_item_id(captured: &CapturedMatch) -> String {
    let mut hash = blake3::Hasher::new();
    hash.update(ITEM_ID_DOMAIN);
    hash_bytes(&mut hash, captured.insertion.as_slice());
    hash.update(&captured.replace_range.start.0.to_le_bytes());
    hash.update(&captured.replace_range.end.0.to_le_bytes());
    hash.update(&[completion_kind_tag(captured.kind)]);
    if let Some(group) = &captured.group {
        hash_bytes(&mut hash, group.name.as_bytes());
    }
    format!("zsh:{}", hash.finalize().to_hex())
}

fn hash_bytes(hash: &mut blake3::Hasher, bytes: &[u8]) {
    hash.update(&u64::try_from(bytes.len()).unwrap_or(u64::MAX).to_le_bytes());
    hash.update(bytes);
}

const fn backend_tag(backend: CaptureBackend) -> u8 {
    match backend {
        CaptureBackend::Native => 1,
        CaptureBackend::Portable => 2,
    }
}

const fn completion_kind_tag(kind: CompletionKind) -> u8 {
    match kind {
        CompletionKind::Text => 0,
        CompletionKind::Command => 1,
        CompletionKind::Alias => 2,
        CompletionKind::Builtin => 3,
        CompletionKind::Function => 4,
        CompletionKind::Subcommand => 5,
        CompletionKind::Option => 6,
        CompletionKind::OptionValue => 7,
        CompletionKind::Variable => 8,
        CompletionKind::File => 9,
        CompletionKind::Directory => 10,
        CompletionKind::Symlink => 11,
        CompletionKind::User => 12,
        CompletionKind::Host => 13,
        CompletionKind::Process => 14,
        CompletionKind::Job => 15,
        CompletionKind::GitBranch => 16,
        CompletionKind::GitTag => 17,
        CompletionKind::GitCommit => 18,
        CompletionKind::Service => 19,
        CompletionKind::Container => 20,
        CompletionKind::Image => 21,
        CompletionKind::Package => 22,
        CompletionKind::History => 23,
        CompletionKind::Snippet => 24,
        CompletionKind::Action => 25,
    }
}

fn capture_size(captured: &CapturedMatch) -> usize {
    let metadata = &captured.insertion_metadata;
    [
        captured.insertion.len(),
        captured.backend_identity.len(),
        metadata.prefix.len(),
        metadata.suffix.len(),
        metadata.hidden_prefix.len(),
        metadata.hidden_suffix.len(),
        metadata.ignored_prefix.len(),
        metadata.ignored_suffix.len(),
        metadata.path_prefix.len(),
        metadata.path_suffix.len(),
        metadata.path_directory.len(),
        metadata.removable_suffix_characters.len(),
        metadata.suffix_removal_function.len(),
        metadata.matcher_specs.iter().map(RawBytes::len).sum(),
        captured.display.as_ref().map_or(0, String::len),
        captured.description.as_ref().map_or(0, String::len),
        captured.explanation.as_ref().map_or(0, String::len),
        captured.group.as_ref().map_or(0, |group| {
            group.name.len() + group.description.as_ref().map_or(0, String::len)
        }),
    ]
    .into_iter()
    .fold(0, usize::saturating_add)
}

fn sanitize_ui(input: &str, max_bytes: usize) -> String {
    let mut output = String::new();
    for character in input.chars() {
        let escaped = if character.is_control() {
            character.escape_default().to_string()
        } else {
            character.to_string()
        };
        if output.len().saturating_add(escaped.len()) > max_bytes {
            break;
        }
        output.push_str(&escaped);
    }
    output
}
