//! Shared, transport-independent data model for Shell Sense.
//!
//! The model deliberately resembles LSP where the semantics overlap, while
//! preserving shell-owned insertion identity and arbitrary Unix bytes.

mod completion;
mod context;
mod intelligence;
mod text;

pub use completion::*;
pub use context::*;
pub use intelligence::*;
pub use text::*;

/// Version of the in-memory model contract.
pub const MODEL_VERSION: u16 = 4;
