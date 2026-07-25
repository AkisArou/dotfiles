use std::io;
use std::marker::PhantomData;

use bytes::BytesMut;
use serde::Serialize;
use serde::de::DeserializeOwned;
use thiserror::Error;
use tokio_util::codec::{Decoder, Encoder, LengthDelimitedCodec};

use crate::DEFAULT_MAX_FRAME_BYTES;

#[derive(Debug, Error)]
pub enum ProtocolError {
    #[error("protocol I/O failed: {0}")]
    Io(#[from] io::Error),
    #[error("message encoding failed: {0}")]
    Encode(#[from] rmp_serde::encode::Error),
    #[error("message decoding failed: {0}")]
    Decode(#[from] rmp_serde::decode::Error),
    #[error("encoded message is {actual} bytes; limit is {limit} bytes")]
    FrameTooLarge { actual: usize, limit: usize },
}

/// A typed `MessagePack` codec with a four-byte big-endian length prefix.
///
/// `Incoming` and `Outgoing` are different so each side can expose only the
/// messages valid in that direction.
#[derive(Debug)]
pub struct MessagePackCodec<Incoming, Outgoing> {
    frames: LengthDelimitedCodec,
    max_frame_bytes: usize,
    marker: PhantomData<fn() -> (Incoming, Outgoing)>,
}

impl<Incoming, Outgoing> MessagePackCodec<Incoming, Outgoing> {
    #[must_use]
    pub fn new(max_frame_bytes: usize) -> Self {
        let frames = LengthDelimitedCodec::builder()
            .length_field_length(4)
            .max_frame_length(max_frame_bytes)
            .new_codec();
        Self {
            frames,
            max_frame_bytes,
            marker: PhantomData,
        }
    }

    #[must_use]
    pub const fn max_frame_bytes(&self) -> usize {
        self.max_frame_bytes
    }
}

impl<Incoming, Outgoing> Default for MessagePackCodec<Incoming, Outgoing> {
    fn default() -> Self {
        Self::new(DEFAULT_MAX_FRAME_BYTES)
    }
}

impl<Incoming, Outgoing> Decoder for MessagePackCodec<Incoming, Outgoing>
where
    Incoming: DeserializeOwned,
{
    type Item = Incoming;
    type Error = ProtocolError;

    fn decode(&mut self, source: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let Some(frame) = self.frames.decode(source)? else {
            return Ok(None);
        };
        Ok(Some(rmp_serde::from_slice(&frame)?))
    }
}

impl<Incoming, Outgoing> Encoder<Outgoing> for MessagePackCodec<Incoming, Outgoing>
where
    Outgoing: Serialize,
{
    type Error = ProtocolError;

    fn encode(&mut self, item: Outgoing, destination: &mut BytesMut) -> Result<(), Self::Error> {
        let payload = rmp_serde::to_vec_named(&item)?;
        if payload.len() > self.max_frame_bytes {
            return Err(ProtocolError::FrameTooLarge {
                actual: payload.len(),
                limit: self.max_frame_bytes,
            });
        }
        self.frames.encode(payload.into(), destination)?;
        Ok(())
    }
}
