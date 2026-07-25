use bytes::BytesMut;
use sense_protocol::{
    ClientHello, ClientMessage, MessagePackCodec, PeerRole, ProtocolError, ProtocolVersion,
};
use tokio_util::codec::{Decoder, Encoder};

#[test]
fn round_trip_preserves_typed_message() {
    let message = ClientMessage::Hello(ClientHello {
        protocol: ProtocolVersion::CURRENT,
        client_version: "0.1.0".into(),
        role: PeerRole::ZleClient,
        process_id: 42,
        zsh: None,
        attach_session: None,
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
        role: PeerRole::ZleClient,
        process_id: 42,
        zsh: None,
        attach_session: None,
    });
    let mut codec = MessagePackCodec::<ClientMessage, ClientMessage>::new(32);
    let error = codec.encode(message, &mut BytesMut::new()).unwrap_err();
    assert!(matches!(error, ProtocolError::FrameTooLarge { .. }));
}
