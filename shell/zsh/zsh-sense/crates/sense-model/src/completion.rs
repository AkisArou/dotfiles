use bitflags::bitflags;
use serde::{Deserialize, Serialize};

use crate::{RawBytes, TextEdit};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ItemId(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SourceId(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct GroupId(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    History,
    Snippet,
    Action,
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
        const PREVIEW = 1 << 1;
        const PARTIAL_ACCEPT = 1 << 2;
        const COMMIT_CHARACTERS = 1 << 3;
        const CODE_ACTIONS = 1 << 4;
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum InsertStrategy {
    /// Selection is replayed through live Zsh completion using this opaque
    /// adapter-generated fingerprint.
    ZshMatch {
        fingerprint: RawBytes,
    },
    TextEdit,
    Snippet {
        body: String,
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
    /// Provider-owned data returned unchanged to resolve/preview requests.
    pub opaque_data: RawBytes,
}

impl CompletionItem {
    #[must_use]
    pub fn plain(
        id: impl Into<String>,
        source: impl Into<String>,
        label: impl Into<String>,
        edit: TextEdit,
    ) -> Self {
        Self {
            id: ItemId(id.into()),
            source: SourceId(source.into()),
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
            insertion: InsertStrategy::TextEdit,
            commit_characters: Vec::new(),
            original_order: 0,
            provider_relevance: 0,
            confidence: Confidence::Authoritative,
            capabilities: ItemCapabilities::empty(),
            match_result: None,
            opaque_data: RawBytes::default(),
        }
    }
}
