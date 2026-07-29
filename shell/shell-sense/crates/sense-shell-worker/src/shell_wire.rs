//! Bounded, binary-safe transport between an interactive shell and its worker.
//!
//! Shell integrations have no native `MessagePack` implementation and words can contain
//! newlines or invalid UTF-8. A message is therefore a sequence of netstrings:
//! a UTF-8 command, a decimal field count, and that many opaque byte fields.

use std::io;

use bytes::{Buf, BufMut, BytesMut};
use sense_model::RawBytes;
use thiserror::Error;
use tokio_util::codec::{Decoder, Encoder};

const MAX_LENGTH_DIGITS: usize = 20;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ShellWireLimits {
    pub max_fields: usize,
    pub max_field_bytes: usize,
    pub max_message_bytes: usize,
    pub max_command_bytes: usize,
}

impl Default for ShellWireLimits {
    fn default() -> Self {
        Self {
            max_fields: 128,
            max_field_bytes: 16 * 1024 * 1024,
            max_message_bytes: 64 * 1024 * 1024,
            max_command_bytes: 64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShellWireMessage {
    pub command: String,
    pub fields: Vec<RawBytes>,
}

impl ShellWireMessage {
    #[must_use]
    pub fn new(command: impl Into<String>, fields: Vec<RawBytes>) -> Self {
        Self {
            command: command.into(),
            fields,
        }
    }
}

#[derive(Debug, Error)]
pub enum ShellWireError {
    #[error("shell wire I/O failed: {0}")]
    Io(#[from] io::Error),
    #[error("shell wire limits must all be greater than zero")]
    InvalidLimits,
    #[error("netstring length is malformed")]
    MalformedLength,
    #[error("netstring terminator is missing")]
    MissingTerminator,
    #[error("field is {actual} bytes; limit is {limit} bytes")]
    FieldTooLarge { actual: usize, limit: usize },
    #[error("message is larger than the {limit}-byte limit")]
    MessageTooLarge { limit: usize },
    #[error("message has {actual} fields; limit is {limit}")]
    TooManyFields { actual: usize, limit: usize },
    #[error("command must be non-empty lowercase ASCII with optional digits and hyphens")]
    InvalidCommand,
    #[error("field count is not a canonical decimal integer")]
    InvalidFieldCount,
}

#[derive(Debug, Clone)]
pub struct ShellWireCodec {
    limits: ShellWireLimits,
}

impl ShellWireCodec {
    /// Construct a shell codec with explicit resource limits.
    ///
    /// # Errors
    ///
    /// Returns [`ShellWireError::InvalidLimits`] when any limit is zero or a
    /// subordinate limit is larger than the whole-message limit.
    pub fn new(limits: ShellWireLimits) -> Result<Self, ShellWireError> {
        if limits.max_fields == 0
            || limits.max_field_bytes == 0
            || limits.max_message_bytes == 0
            || limits.max_command_bytes == 0
            || limits.max_field_bytes > limits.max_message_bytes
            || limits.max_command_bytes > limits.max_message_bytes
        {
            return Err(ShellWireError::InvalidLimits);
        }
        Ok(Self { limits })
    }

    #[must_use]
    pub const fn limits(&self) -> ShellWireLimits {
        self.limits
    }
}

impl Default for ShellWireCodec {
    fn default() -> Self {
        Self::new(ShellWireLimits::default()).expect("default shell wire limits are valid")
    }
}

impl Decoder for ShellWireCodec {
    type Item = ShellWireMessage;
    type Error = ShellWireError;

    fn decode(&mut self, source: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let Some((command_bytes, after_command)) =
            parse_netstring(source, 0, self.limits.max_command_bytes)?
        else {
            reject_oversized_incomplete(source, self.limits.max_message_bytes)?;
            return Ok(None);
        };
        if !valid_command(command_bytes) {
            return Err(ShellWireError::InvalidCommand);
        }

        let Some((count_bytes, mut offset)) =
            parse_netstring(source, after_command, MAX_LENGTH_DIGITS)?
        else {
            reject_oversized_incomplete(source, self.limits.max_message_bytes)?;
            return Ok(None);
        };
        let field_count =
            parse_canonical_usize(count_bytes).ok_or(ShellWireError::InvalidFieldCount)?;
        if field_count > self.limits.max_fields {
            return Err(ShellWireError::TooManyFields {
                actual: field_count,
                limit: self.limits.max_fields,
            });
        }

        let mut fields = Vec::with_capacity(field_count);
        for _ in 0..field_count {
            let Some((field, next)) = parse_netstring(source, offset, self.limits.max_field_bytes)?
            else {
                reject_oversized_incomplete(source, self.limits.max_message_bytes)?;
                return Ok(None);
            };
            offset = next;
            if offset > self.limits.max_message_bytes {
                return Err(ShellWireError::MessageTooLarge {
                    limit: self.limits.max_message_bytes,
                });
            }
            fields.push(RawBytes::from(field));
        }

        let command = String::from_utf8(command_bytes.to_vec())
            .map_err(|_| ShellWireError::InvalidCommand)?;
        source.advance(offset);
        Ok(Some(ShellWireMessage { command, fields }))
    }
}

impl Encoder<ShellWireMessage> for ShellWireCodec {
    type Error = ShellWireError;

    fn encode(
        &mut self,
        message: ShellWireMessage,
        destination: &mut BytesMut,
    ) -> Result<(), Self::Error> {
        let command = message.command.as_bytes();
        if command.len() > self.limits.max_command_bytes || !valid_command(command) {
            return Err(ShellWireError::InvalidCommand);
        }
        if message.fields.len() > self.limits.max_fields {
            return Err(ShellWireError::TooManyFields {
                actual: message.fields.len(),
                limit: self.limits.max_fields,
            });
        }
        for field in &message.fields {
            if field.len() > self.limits.max_field_bytes {
                return Err(ShellWireError::FieldTooLarge {
                    actual: field.len(),
                    limit: self.limits.max_field_bytes,
                });
            }
        }

        let count = message.fields.len().to_string();
        let encoded_len = netstring_size(command.len())
            .and_then(|size| size.checked_add(netstring_size(count.len())?))
            .and_then(|size| {
                message.fields.iter().try_fold(size, |total, field| {
                    total.checked_add(netstring_size(field.len())?)
                })
            })
            .ok_or(ShellWireError::MessageTooLarge {
                limit: self.limits.max_message_bytes,
            })?;
        if encoded_len > self.limits.max_message_bytes {
            return Err(ShellWireError::MessageTooLarge {
                limit: self.limits.max_message_bytes,
            });
        }

        destination.reserve(encoded_len);
        encode_netstring(command, destination);
        encode_netstring(count.as_bytes(), destination);
        for field in message.fields {
            encode_netstring(field.as_slice(), destination);
        }
        Ok(())
    }
}

fn parse_netstring(
    source: &[u8],
    start: usize,
    max_payload_bytes: usize,
) -> Result<Option<(&[u8], usize)>, ShellWireError> {
    if start >= source.len() {
        return Ok(None);
    }
    let tail = &source[start..];
    let Some(colon_relative) = tail.iter().position(|byte| *byte == b':') else {
        if tail.len() > MAX_LENGTH_DIGITS {
            return Err(ShellWireError::MalformedLength);
        }
        if tail.iter().any(|byte| !byte.is_ascii_digit()) {
            return Err(ShellWireError::MalformedLength);
        }
        return Ok(None);
    };
    if colon_relative == 0 || colon_relative > MAX_LENGTH_DIGITS {
        return Err(ShellWireError::MalformedLength);
    }
    let length_bytes = &tail[..colon_relative];
    let length = parse_canonical_usize(length_bytes).ok_or(ShellWireError::MalformedLength)?;
    if length > max_payload_bytes {
        return Err(ShellWireError::FieldTooLarge {
            actual: length,
            limit: max_payload_bytes,
        });
    }
    let payload_start = start
        .checked_add(colon_relative)
        .and_then(|value| value.checked_add(1))
        .ok_or(ShellWireError::MalformedLength)?;
    let payload_end = payload_start
        .checked_add(length)
        .ok_or(ShellWireError::MalformedLength)?;
    let terminator = payload_end
        .checked_add(1)
        .ok_or(ShellWireError::MalformedLength)?;
    if source.len() < terminator {
        return Ok(None);
    }
    if source[payload_end] != b',' {
        return Err(ShellWireError::MissingTerminator);
    }
    Ok(Some((&source[payload_start..payload_end], terminator)))
}

fn parse_canonical_usize(bytes: &[u8]) -> Option<usize> {
    if bytes.is_empty()
        || bytes.iter().any(|byte| !byte.is_ascii_digit())
        || (bytes.len() > 1 && bytes[0] == b'0')
    {
        return None;
    }
    bytes.iter().try_fold(0_usize, |value, digit| {
        value
            .checked_mul(10)?
            .checked_add(usize::from(*digit - b'0'))
    })
}

fn valid_command(command: &[u8]) -> bool {
    !command.is_empty()
        && command
            .iter()
            .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(*byte, b'-'))
}

fn netstring_size(payload_len: usize) -> Option<usize> {
    payload_len
        .checked_add(payload_len.to_string().len())?
        .checked_add(2)
}

fn encode_netstring(payload: &[u8], destination: &mut BytesMut) {
    destination.put_slice(payload.len().to_string().as_bytes());
    destination.put_u8(b':');
    destination.put_slice(payload);
    destination.put_u8(b',');
}

fn reject_oversized_incomplete(
    source: &[u8],
    max_message_bytes: usize,
) -> Result<(), ShellWireError> {
    if source.len() > max_message_bytes {
        Err(ShellWireError::MessageTooLarge {
            limit: max_message_bytes,
        })
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn encode(message: ShellWireMessage) -> BytesMut {
        let mut bytes = BytesMut::new();
        ShellWireCodec::default()
            .encode(message, &mut bytes)
            .unwrap();
        bytes
    }

    #[test]
    fn round_trips_arbitrary_field_bytes() {
        let expected = ShellWireMessage::new(
            "candidate",
            vec![
                RawBytes::from(&b"line one\nline two\xff\0"[..]),
                RawBytes::default(),
            ],
        );
        let mut encoded = encode(expected.clone());
        let actual = ShellWireCodec::default().decode(&mut encoded).unwrap();
        assert_eq!(actual, Some(expected));
        assert!(encoded.is_empty());
    }

    #[test]
    fn decodes_every_possible_two_chunk_split() {
        let expected = ShellWireMessage::new(
            "complete",
            vec![RawBytes::from("systemctl rstart"), RawBytes::from("/tmp")],
        );
        let encoded = encode(expected.clone()).freeze();
        for split in 0..=encoded.len() {
            let mut codec = ShellWireCodec::default();
            let mut input = BytesMut::new();
            input.extend_from_slice(&encoded[..split]);
            let first = codec.decode(&mut input).unwrap();
            if split < encoded.len() {
                assert!(first.is_none(), "split {split} decoded prematurely");
                input.extend_from_slice(&encoded[split..]);
                assert_eq!(codec.decode(&mut input).unwrap(), Some(expected.clone()));
            } else {
                assert_eq!(first, Some(expected.clone()));
            }
            assert!(input.is_empty());
        }
    }

    #[test]
    fn preserves_following_message() {
        let first = ShellWireMessage::new("ping", vec![RawBytes::from("1")]);
        let second = ShellWireMessage::new("goodbye", vec![]);
        let mut input = encode(first.clone());
        input.extend_from_slice(&encode(second.clone()));
        let mut codec = ShellWireCodec::default();
        assert_eq!(codec.decode(&mut input).unwrap(), Some(first));
        assert_eq!(codec.decode(&mut input).unwrap(), Some(second));
        assert!(input.is_empty());
    }

    #[test]
    fn rejects_noncanonical_and_malformed_netstrings() {
        let mut leading_zero = BytesMut::from(&b"04:ping,1:0,"[..]);
        assert!(matches!(
            ShellWireCodec::default().decode(&mut leading_zero),
            Err(ShellWireError::MalformedLength)
        ));

        let mut missing_comma = BytesMut::from(&b"4:ping;1:0,"[..]);
        assert!(matches!(
            ShellWireCodec::default().decode(&mut missing_comma),
            Err(ShellWireError::MissingTerminator)
        ));

        let mut bad_count = BytesMut::from(&b"4:ping,2:01,"[..]);
        assert!(matches!(
            ShellWireCodec::default().decode(&mut bad_count),
            Err(ShellWireError::InvalidFieldCount)
        ));
    }

    #[test]
    fn enforces_declared_and_accumulated_bounds() {
        let limits = ShellWireLimits {
            max_fields: 2,
            max_field_bytes: 4,
            max_message_bytes: 20,
            max_command_bytes: 8,
        };
        let mut too_many = BytesMut::from(&b"4:ping,1:3,"[..]);
        assert!(matches!(
            ShellWireCodec::new(limits).unwrap().decode(&mut too_many),
            Err(ShellWireError::TooManyFields { .. })
        ));

        let mut field_too_large = BytesMut::from(&b"4:ping,1:1,5:"[..]);
        assert!(matches!(
            ShellWireCodec::new(limits)
                .unwrap()
                .decode(&mut field_too_large),
            Err(ShellWireError::FieldTooLarge { .. })
        ));

        let message =
            ShellWireMessage::new("ping", vec![RawBytes::from("1234"), RawBytes::from("5678")]);
        assert!(matches!(
            ShellWireCodec::new(limits)
                .unwrap()
                .encode(message, &mut BytesMut::new()),
            Err(ShellWireError::MessageTooLarge { .. })
        ));
    }

    #[test]
    fn fragmented_randomish_binary_corpus_is_lossless() {
        let fields: Vec<_> = (0_u16..512)
            .map(|seed| {
                let length = usize::from(seed % 37);
                let bytes = (0..length)
                    .map(|index| {
                        seed.wrapping_mul(109)
                            .wrapping_add(u16::try_from(index).unwrap().wrapping_mul(67))
                            .to_le_bytes()[0]
                    })
                    .collect::<Vec<_>>();
                RawBytes::from(bytes)
            })
            .collect();
        let expected = ShellWireMessage::new("candidate-batch", fields);
        let limits = ShellWireLimits {
            max_fields: 1_024,
            ..ShellWireLimits::default()
        };
        let mut encoded = BytesMut::new();
        ShellWireCodec::new(limits)
            .unwrap()
            .encode(expected.clone(), &mut encoded)
            .unwrap();

        let source = encoded.freeze();
        let mut accumulated = BytesMut::new();
        let mut codec = ShellWireCodec::new(limits).unwrap();
        let mut offset = 0;
        let mut stride = 1_usize;
        let mut decoded = None;
        while offset < source.len() {
            let end = (offset + stride).min(source.len());
            accumulated.extend_from_slice(&source[offset..end]);
            decoded = codec.decode(&mut accumulated).unwrap().or(decoded);
            offset = end;
            stride = stride.wrapping_mul(17) % 113 + 1;
        }
        assert_eq!(decoded, Some(expected));
        assert!(accumulated.is_empty());
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(256))]

        #[test]
        fn arbitrary_messages_survive_fragmentation(
            command in "[a-z][a-z0-9-]{0,31}",
            fields in prop::collection::vec(
                prop::collection::vec(any::<u8>(), 0..256),
                0..32,
            ),
            chunk_sizes in prop::collection::vec(1_usize..128, 1..32),
        ) {
            let message = ShellWireMessage::new(
                command,
                fields.into_iter().map(RawBytes::from).collect(),
            );
            let encoded = encode(message.clone()).freeze();
            let mut codec = ShellWireCodec::default();
            let mut input = BytesMut::new();
            let mut offset = 0;
            let mut chunk_index = 0;
            let mut decoded = None;

            while offset < encoded.len() {
                let chunk_size = chunk_sizes[chunk_index % chunk_sizes.len()];
                let end = (offset + chunk_size).min(encoded.len());
                input.extend_from_slice(&encoded[offset..end]);
                let candidate = codec.decode(&mut input).unwrap();
                if end < encoded.len() {
                    prop_assert!(candidate.is_none());
                } else {
                    decoded = candidate;
                }
                offset = end;
                chunk_index += 1;
            }

            prop_assert_eq!(decoded, Some(message));
            prop_assert!(input.is_empty());
        }

        #[test]
        fn arbitrary_wire_bytes_are_panic_free(
            bytes in prop::collection::vec(any::<u8>(), 0..8192),
        ) {
            let mut codec = ShellWireCodec::default();
            let mut input = BytesMut::from(bytes.as_slice());
            let original_length = input.len();
            let _ = codec.decode(&mut input);
            prop_assert!(input.len() <= original_length);
        }
    }
}
