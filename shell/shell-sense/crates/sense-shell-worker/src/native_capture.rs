//! Capture storage shared by Fish and Bash native completion adapters.

use std::collections::HashMap;

use sense_model::{
    CompletionKind, CompletionResource, Confidence, DocumentationState, Generation, GroupId,
    ItemCapabilities, ItemId, ItemTags, NativeShell, RawBytes, RequestId, SourceId, TextEdit,
    TextRange,
};
use sense_protocol::CandidateBatch;
use sense_provider_api::NativeCandidate;

use crate::{CaptureError, CaptureLimits, CaptureOutcome};

const FINGERPRINT_DOMAIN: &[u8] = b"shell-sense/native-fingerprint/v1";
const ITEM_ID_DOMAIN: &[u8] = b"shell-sense/native-item/v1";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShellCapturedMatch {
    pub insertion: RawBytes,
    pub label: String,
    pub description: Option<String>,
    pub group: Option<String>,
    pub kind: CompletionKind,
    pub resource_path: Option<RawBytes>,
    pub replace_range: TextRange,
    pub original_order: u32,
    pub append_space: bool,
    pub partial_accept: bool,
    pub acceptance_identity: RawBytes,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShellAcceptanceRoute {
    pub shell: NativeShell,
    pub request_id: RequestId,
    pub generation: Generation,
    pub insertion: RawBytes,
    pub replace_range: TextRange,
    pub append_space: bool,
    pub acceptance_identity: RawBytes,
}

#[derive(Debug)]
struct ActiveCapture {
    request_id: RequestId,
    generation: Generation,
    routes: HashMap<ItemId, ShellAcceptanceRoute>,
}

#[derive(Debug)]
pub struct ShellCaptureStore {
    shell: NativeShell,
    limits: CaptureLimits,
    active: Option<ActiveCapture>,
}

impl ShellCaptureStore {
    /// Create a bounded store for one non-Zsh native provider.
    ///
    /// # Errors
    ///
    /// Zsh uses its metadata-complete capture store. Zero limits are rejected.
    pub fn new(shell: NativeShell, limits: CaptureLimits) -> Result<Self, CaptureError> {
        if shell == NativeShell::Zsh {
            return Err(CaptureError::InvalidShellStore);
        }
        if limits.max_candidates == 0 || limits.max_bytes == 0 || limits.max_ui_bytes_per_field == 0
        {
            return Err(CaptureError::InvalidLimits);
        }
        Ok(Self {
            shell,
            limits,
            active: None,
        })
    }

    /// Replace the active generation and normalize native matches.
    ///
    /// # Errors
    ///
    /// Returns an error when a replacement range is outside the original
    /// command buffer.
    pub fn install(
        &mut self,
        request: &sense_model::CompletionRequest,
        matches: Vec<ShellCapturedMatch>,
    ) -> Result<CaptureOutcome, CaptureError> {
        for (index, candidate) in matches.iter().enumerate() {
            if !candidate
                .replace_range
                .is_valid_for(request.buffer.as_slice())
            {
                return Err(CaptureError::InvalidRange { index });
            }
        }

        let total = matches.len();
        let mut captured_bytes = 0_usize;
        let mut items = Vec::new();
        let mut routes = HashMap::new();
        for candidate in matches {
            let candidate_size = shell_capture_size(&candidate);
            if items.len() == self.limits.max_candidates
                || captured_bytes.saturating_add(candidate_size) > self.limits.max_bytes
            {
                break;
            }
            captured_bytes += candidate_size;
            let fingerprint = fingerprint(request, self.shell, &candidate);
            let item_id = item_id(self.shell, &candidate);
            let mut capabilities = ItemCapabilities::empty();
            if candidate.partial_accept {
                capabilities.insert(ItemCapabilities::PARTIAL_ACCEPT);
            }
            if candidate.kind == CompletionKind::Option {
                capabilities.insert(ItemCapabilities::RESOLVE_DOCUMENTATION);
            }
            let detail = candidate
                .description
                .as_deref()
                .map(|value| sanitize_ui(value, self.limits.max_ui_bytes_per_field));
            let item = NativeCandidate {
                id: item_id.clone(),
                label: sanitize_ui(&candidate.label, self.limits.max_ui_bytes_per_field),
                label_detail: None,
                filter_text: Some(candidate.insertion.display_lossy()),
                kind: candidate.kind,
                tags: ItemTags::empty(),
                documentation: DocumentationState::None,
                detail,
                group: candidate.group.as_deref().map(|group| {
                    GroupId(format!(
                        "{}:{}",
                        self.shell.source_name(),
                        sanitize_ui(group, self.limits.max_ui_bytes_per_field)
                    ))
                }),
                edit: TextEdit::new(candidate.replace_range, candidate.insertion.clone()),
                acceptance: fingerprint,
                original_order: candidate.original_order,
                confidence: Confidence::Authoritative,
                capabilities,
                resource: candidate
                    .resource_path
                    .clone()
                    .map(|path| CompletionResource::FileSystemPath { path }),
                opaque_data: RawBytes::default(),
            }
            .into_completion_item(self.shell);
            routes.insert(
                item_id,
                ShellAcceptanceRoute {
                    shell: self.shell,
                    request_id: request.request_id,
                    generation: request.generation,
                    insertion: candidate.insertion,
                    replace_range: candidate.replace_range,
                    append_space: candidate.append_space,
                    acceptance_identity: candidate.acceptance_identity,
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
        });
        Ok(CaptureOutcome {
            batch: CandidateBatch {
                session_id: request.session_id,
                request_id: request.request_id,
                generation: request.generation,
                source: SourceId(self.shell.source_name().into()),
                items,
                is_final: true,
                is_incomplete: dropped != 0,
            },
            captured,
            dropped,
            captured_bytes,
        })
    }

    /// Resolve an item only while its capture generation remains active.
    ///
    /// # Errors
    ///
    /// Rejects stale generations and unknown item identifiers.
    pub fn acceptance_by_item(
        &self,
        request_id: RequestId,
        generation: Generation,
        item_id: &ItemId,
    ) -> Result<&ShellAcceptanceRoute, CaptureError> {
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
            .get(item_id)
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

fn fingerprint(
    request: &sense_model::CompletionRequest,
    shell: NativeShell,
    candidate: &ShellCapturedMatch,
) -> RawBytes {
    let mut hash = blake3::Hasher::new();
    hash.update(FINGERPRINT_DOMAIN);
    hash.update(request.session_id.0.as_bytes());
    hash.update(&request.request_id.0.to_le_bytes());
    hash.update(&request.generation.0.to_le_bytes());
    hash.update(shell.source_name().as_bytes());
    hash_bytes(&mut hash, candidate.acceptance_identity.as_slice());
    hash_bytes(&mut hash, candidate.insertion.as_slice());
    RawBytes::from(hash.finalize().as_bytes().as_slice())
}

fn item_id(shell: NativeShell, candidate: &ShellCapturedMatch) -> ItemId {
    let mut hash = blake3::Hasher::new();
    hash.update(ITEM_ID_DOMAIN);
    hash.update(shell.source_name().as_bytes());
    hash_bytes(&mut hash, candidate.acceptance_identity.as_slice());
    hash_bytes(&mut hash, candidate.insertion.as_slice());
    hash.update(&candidate.replace_range.start.0.to_le_bytes());
    hash.update(&candidate.replace_range.end.0.to_le_bytes());
    ItemId(hash.finalize().to_hex().to_string())
}

fn hash_bytes(hash: &mut blake3::Hasher, value: &[u8]) {
    hash.update(&u64::try_from(value.len()).unwrap_or(u64::MAX).to_le_bytes());
    hash.update(value);
}

pub(crate) fn shell_capture_size(candidate: &ShellCapturedMatch) -> usize {
    [
        candidate.insertion.len(),
        candidate.label.len(),
        candidate.description.as_ref().map_or(0, String::len),
        candidate.group.as_ref().map_or(0, String::len),
        candidate.resource_path.as_ref().map_or(0, RawBytes::len),
        candidate.acceptance_identity.len(),
    ]
    .into_iter()
    .fold(0, usize::saturating_add)
}

fn sanitize_ui(input: &str, maximum_bytes: usize) -> String {
    let mut output = String::new();
    for character in input.chars() {
        let escaped = if character.is_control() {
            character.escape_default().to_string()
        } else {
            character.to_string()
        };
        if output.len().saturating_add(escaped.len()) > maximum_bytes {
            break;
        }
        output.push_str(&escaped);
    }
    output
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use sense_model::{
        ByteOffset, CompletionRequest, ContextEpoch, TerminalDimensions, TriggerKind,
    };

    use super::*;

    fn request(generation: u64) -> CompletionRequest {
        CompletionRequest {
            session_id: sense_model::SessionId::new(),
            request_id: RequestId(7),
            generation: Generation(generation),
            context_epoch: ContextEpoch::default(),
            buffer: RawBytes::from("systemctl rstart"),
            cursor: ByteOffset(16),
            cwd: RawBytes::from("/tmp"),
            keymap: "default".into(),
            terminal: TerminalDimensions::default(),
            trigger: TriggerKind::Automatic,
            environment: BTreeMap::new(),
        }
    }

    fn candidate(label: &str) -> ShellCapturedMatch {
        ShellCapturedMatch {
            insertion: RawBytes::from(label),
            label: label.into(),
            description: Some("systemd service command".into()),
            group: Some("subcommands".into()),
            kind: CompletionKind::Subcommand,
            resource_path: None,
            replace_range: TextRange::new(10, 16),
            original_order: 0,
            append_space: true,
            partial_accept: false,
            acceptance_identity: RawBytes::from(label),
        }
    }

    #[test]
    fn fish_capture_is_normalized_and_accepted_only_in_its_generation() {
        let first = request(1);
        let mut store =
            ShellCaptureStore::new(NativeShell::Fish, CaptureLimits::default()).unwrap();
        let outcome = store.install(&first, vec![candidate("restart")]).unwrap();
        assert_eq!(outcome.batch.source.0, "fish");
        let item = &outcome.batch.items[0];
        assert_eq!(item.detail.as_deref(), Some("systemd service command"));
        assert_eq!(item.documentation, DocumentationState::None);
        assert!(matches!(
            item.insertion,
            sense_model::InsertStrategy::NativeMatch {
                shell: NativeShell::Fish,
                ..
            }
        ));
        assert_eq!(
            store
                .acceptance_by_item(first.request_id, first.generation, &item.id)
                .unwrap()
                .insertion
                .as_slice(),
            b"restart"
        );

        let mut second = request(2);
        second.session_id = first.session_id;
        store
            .install(&second, vec![candidate("reset-failed")])
            .unwrap();
        assert!(matches!(
            store.acceptance_by_item(first.request_id, first.generation, &item.id),
            Err(CaptureError::StaleCapture { .. })
        ));
    }

    #[test]
    fn native_options_are_documentation_resolvable_before_daemon_ranking() {
        let request = request(1);
        let mut option = candidate("--recursive");
        option.kind = CompletionKind::Option;
        let mut store =
            ShellCaptureStore::new(NativeShell::Bash, CaptureLimits::default()).unwrap();
        let outcome = store.install(&request, vec![option]).unwrap();
        assert!(
            outcome.batch.items[0]
                .capabilities
                .contains(ItemCapabilities::RESOLVE_DOCUMENTATION)
        );
    }
}
