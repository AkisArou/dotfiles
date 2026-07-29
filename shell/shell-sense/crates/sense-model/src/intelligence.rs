use serde::{Deserialize, Serialize};

use crate::{
    CompletionKind, DocumentationState, ItemCapabilities, ItemId, ItemTags, SourceId, TextEdit,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enrichment {
    pub item_id: ItemId,
    pub kind: Option<CompletionKind>,
    pub add_tags: ItemTags,
    pub add_capabilities: ItemCapabilities,
    pub detail: Option<String>,
    pub documentation: Option<DocumentationState>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "payload", rename_all = "kebab-case")]
pub enum AdapterEvent {
    Enrichments(Vec<Enrichment>),
    Documentation {
        item_id: ItemId,
        documentation: DocumentationState,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GhostText {
    pub edit: TextEdit,
    pub source: SourceId,
    pub confidence: f32,
}
