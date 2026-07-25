//! Shared, transport-independent data model for zsh-sense.
//!
//! The model deliberately resembles LSP where the semantics overlap, while
//! preserving Zsh-specific insertion identity and arbitrary Unix bytes.

mod completion;
mod context;
mod intelligence;
mod text;

pub use completion::*;
pub use context::*;
pub use intelligence::*;
pub use text::*;

/// Version of the in-memory model contract.
pub const MODEL_VERSION: u16 = 1;
