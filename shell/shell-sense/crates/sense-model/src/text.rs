use std::fmt;

use serde::{Deserialize, Serialize};

/// Bytes that are not required to be valid UTF-8.
///
/// Unix filenames and Zsh words may contain arbitrary bytes. Display text is
/// stored separately and must never be used to reconstruct insertion bytes.
#[derive(Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RawBytes(#[serde(with = "serde_bytes")] pub Vec<u8>);

impl RawBytes {
    #[must_use]
    pub fn new(bytes: impl Into<Vec<u8>>) -> Self {
        Self(bytes.into())
    }

    #[must_use]
    pub fn as_slice(&self) -> &[u8] {
        &self.0
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Produce a safe diagnostic label. This value is never suitable for
    /// insertion back into the shell.
    #[must_use]
    pub fn display_lossy(&self) -> String {
        String::from_utf8_lossy(&self.0).into_owned()
    }
}

impl fmt::Debug for RawBytes {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_tuple("RawBytes")
            .field(&self.display_lossy())
            .finish()
    }
}

impl From<Vec<u8>> for RawBytes {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl From<&[u8]> for RawBytes {
    fn from(value: &[u8]) -> Self {
        Self(value.to_vec())
    }
}

impl From<String> for RawBytes {
    fn from(value: String) -> Self {
        Self(value.into_bytes())
    }
}

impl From<&str> for RawBytes {
    fn from(value: &str) -> Self {
        Self(value.as_bytes().to_vec())
    }
}

/// A byte offset into the command buffer.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct ByteOffset(pub u32);

impl ByteOffset {
    #[must_use]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// A half-open byte range into the command buffer.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TextRange {
    pub start: ByteOffset,
    pub end: ByteOffset,
}

impl TextRange {
    #[must_use]
    pub const fn new(start: u32, end: u32) -> Self {
        Self {
            start: ByteOffset(start),
            end: ByteOffset(end),
        }
    }

    #[must_use]
    pub fn is_valid_for(self, bytes: &[u8]) -> bool {
        self.start <= self.end && self.end.as_usize() <= bytes.len()
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.start.0 == self.end.0
    }
}

/// An explicit replacement in the command buffer.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextEdit {
    pub range: TextRange,
    pub new_text: RawBytes,
}

impl TextEdit {
    #[must_use]
    pub fn new(range: TextRange, new_text: impl Into<RawBytes>) -> Self {
        Self {
            range,
            new_text: new_text.into(),
        }
    }
}
