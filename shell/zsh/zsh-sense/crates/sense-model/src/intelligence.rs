use serde::{Deserialize, Serialize};

use crate::{Confidence, ItemId, MarkupContent, SourceId, TextEdit, TextRange};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParameterInformation {
    pub label: String,
    pub documentation: Option<MarkupContent>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignatureInformation {
    pub label: String,
    pub documentation: Option<MarkupContent>,
    pub parameters: Vec<ParameterInformation>,
    pub active_parameter: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignatureHelp {
    pub signatures: Vec<SignatureInformation>,
    pub active_signature: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub range: TextRange,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub code: Option<String>,
    pub source: SourceId,
    pub confidence: Confidence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ActionSafety {
    Safe,
    Review,
    Destructive,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CodeAction {
    pub id: String,
    pub title: String,
    pub edits: Vec<TextEdit>,
    pub safety: ActionSafety,
    pub diagnostic_code: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PreviewSection {
    pub title: Option<String>,
    pub content: MarkupContent,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Preview {
    pub item_id: ItemId,
    pub title: String,
    pub sections: Vec<PreviewSection>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GhostText {
    pub edit: TextEdit,
    pub source: SourceId,
    pub confidence: f32,
}
