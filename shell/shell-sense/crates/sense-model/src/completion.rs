use bitflags::bitflags;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{NativeShell, RawBytes, TextEdit};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ItemId(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SourceId(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct GroupId(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum CompletionKind {
    Text,
    Command,
    Alias,
    Builtin,
    Function,
    Subcommand,
    Option,
    OptionValue,
    Variable,
    File,
    Directory,
    Symlink,
    User,
    Host,
    Process,
    Job,
    GitBranch,
    GitTag,
    GitCommit,
    Service,
    Container,
    Image,
    Package,
}

bitflags! {
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct ItemTags: u16 {
        const DEPRECATED = 1 << 0;
        const DANGEROUS = 1 << 1;
        const RECENT = 1 << 2;
        const RUNNING = 1 << 3;
        const HIDDEN = 1 << 4;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct ItemCapabilities: u16 {
        const RESOLVE_DOCUMENTATION = 1 << 0;
        const PARTIAL_ACCEPT = 1 << 1;
        const COMMIT_CHARACTERS = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Confidence {
    Advisory,
    Inferred,
    Partial,
    Authoritative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum MarkupKind {
    PlainText,
    Markdown,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MarkupContent {
    pub kind: MarkupKind,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", content = "content", rename_all = "kebab-case")]
pub enum DocumentationState {
    None,
    Unresolved,
    Resolved(MarkupContent),
}

/// A typed resource represented by a completion item.
///
/// Display labels and insertion text are presentation and shell-editing data;
/// neither is a reliable filesystem operand. Native providers attach the
/// resource explicitly so context adapters never have to reverse shell
/// quoting or reconstruct a path from UI text.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum CompletionResource {
    FileSystemPath { path: RawBytes },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum InsertStrategy {
    /// Selection is replayed through the active shell using this opaque,
    /// request-scoped identity. The shell discriminator prevents an item from
    /// ever being accepted by a different native provider.
    NativeMatch {
        shell: NativeShell,
        fingerprint: RawBytes,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MatchResult {
    pub score: i64,
    pub indices: Vec<u32>,
    pub exact: bool,
    pub prefix: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompletionItem {
    pub id: ItemId,
    pub source: SourceId,
    pub label: String,
    pub label_detail: Option<String>,
    pub filter_text: Option<String>,
    pub sort_text: Option<String>,
    pub kind: CompletionKind,
    pub tags: ItemTags,
    pub detail: Option<String>,
    pub documentation: DocumentationState,
    pub group: Option<GroupId>,
    pub edit: TextEdit,
    pub insertion: InsertStrategy,
    pub commit_characters: Vec<char>,
    pub original_order: u32,
    pub provider_relevance: i32,
    pub confidence: Confidence,
    pub capabilities: ItemCapabilities,
    pub match_result: Option<MatchResult>,
    pub resource: Option<CompletionResource>,
    /// Provider-owned data returned unchanged to documentation resolvers.
    pub opaque_data: RawBytes,
}

impl CompletionItem {
    #[must_use]
    pub fn native(
        id: impl Into<String>,
        shell: NativeShell,
        label: impl Into<String>,
        edit: TextEdit,
        fingerprint: impl Into<RawBytes>,
    ) -> Self {
        Self {
            id: ItemId(id.into()),
            source: SourceId(shell.source_name().into()),
            label: label.into(),
            label_detail: None,
            filter_text: None,
            sort_text: None,
            kind: CompletionKind::Text,
            tags: ItemTags::empty(),
            detail: None,
            documentation: DocumentationState::None,
            group: None,
            edit,
            insertion: InsertStrategy::NativeMatch {
                shell,
                fingerprint: fingerprint.into(),
            },
            commit_characters: Vec::new(),
            original_order: 0,
            provider_relevance: 0,
            confidence: Confidence::Authoritative,
            capabilities: ItemCapabilities::empty(),
            match_result: None,
            resource: None,
            opaque_data: RawBytes::default(),
        }
    }
}
