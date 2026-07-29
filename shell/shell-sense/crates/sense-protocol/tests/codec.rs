use bytes::BytesMut;
use proptest::prelude::*;
use sense_protocol::{
    ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolError, ProtocolVersion,
};
use tokio_util::codec::{Decoder, Encoder};

#[test]
fn round_trip_preserves_typed_message() {
    let message = ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "0.1.0".into(),
        role: PeerRole::ShellClient,
        process_id: 42,
        shell: None,
        attach_session: None,
        attach_process_id: None,
    });
    let mut codec = MessagePackCodec::<ClientMessage, ClientMessage>::new(4096);
    let mut buffer = BytesMut::new();
    codec.encode(message.clone(), &mut buffer).unwrap();
    assert_eq!(codec.decode(&mut buffer).unwrap(), Some(message));
    assert!(buffer.is_empty());
}

#[test]
fn oversized_messages_are_rejected_before_framing() {
    let message = ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "x".repeat(128),
        role: PeerRole::ShellClient,
        process_id: 42,
        shell: None,
        attach_session: None,
        attach_process_id: None,
    });
    let mut codec = MessagePackCodec::<ClientMessage, ClientMessage>::new(32);
    let error = codec.encode(message, &mut BytesMut::new()).unwrap_err();
    assert!(matches!(error, ProtocolError::FrameTooLarge { .. }));
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    #[test]
    fn arbitrary_fragmentation_preserves_messages(
        client_version in prop::collection::vec(any::<char>(), 0..128)
            .prop_map(|characters| characters.into_iter().collect::<String>()),
        process_id in any::<u32>(),
        chunk_sizes in prop::collection::vec(1_usize..64, 1..32),
    ) {
        let message = ClientMessage::Hello(ClientHello {
            protocol: ProtocolVersion::CURRENT,
            client_version,
            role: PeerRole::ShellClient,
            process_id,
            shell: None,
            attach_session: None,
            attach_process_id: None,
        });
        let mut codec = MessagePackCodec::<ClientMessage, ClientMessage>::new(4096);
        let mut encoded = BytesMut::new();
        codec.encode(message.clone(), &mut encoded).unwrap();
        let encoded = encoded.freeze();

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
    fn arbitrary_wire_bytes_are_panic_free(bytes in prop::collection::vec(any::<u8>(), 0..8192)) {
        let mut codec = MessagePackCodec::<ClientMessage, ClientMessage>::new(4096);
        let mut input = BytesMut::from(bytes.as_slice());
        let original_length = input.len();
        let _ = codec.decode(&mut input);
        prop_assert!(input.len() <= original_length);
    }
}
