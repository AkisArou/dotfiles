//! Versioned, bounded `MessagePack` protocol used between zsh-sense processes.

mod codec;
mod message;

pub use codec::*;
pub use message::*;

/// Largest individual frame. Candidate lists larger than this must be split
/// into batches, which provides backpressure and cancellation points.
pub const DEFAULT_MAX_FRAME_BYTES: usize = 16 * 1024 * 1024;
