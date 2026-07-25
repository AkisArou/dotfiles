use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{ByteOffset, RawBytes};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SessionId(pub Uuid);

impl SessionId {
    #[must_use]
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for SessionId {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RequestId(pub u64);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Generation(pub u64);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ContextEpoch(pub [u8; 32]);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TriggerKind {
    Automatic,
    Manual,
    TriggerCharacter,
    AfterAccept,
    IncompleteRefresh,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TerminalDimensions {
    pub columns: u16,
    pub rows: u16,
}

impl Default for TerminalDimensions {
    fn default() -> Self {
        Self {
            columns: 80,
            rows: 24,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompletionRequest {
    pub session_id: SessionId,
    pub request_id: RequestId,
    pub generation: Generation,
    pub context_epoch: ContextEpoch,
    pub buffer: RawBytes,
    pub cursor: ByteOffset,
    pub cwd: RawBytes,
    pub keymap: String,
    pub terminal: TerminalDimensions,
    pub trigger: TriggerKind,
    /// Only explicitly allowlisted environment entries are sent.
    pub environment: BTreeMap<String, RawBytes>,
}

impl CompletionRequest {
    #[must_use]
    pub fn cursor_is_valid(&self) -> bool {
        self.cursor.as_usize() <= self.buffer.len()
    }
}
