use std::collections::BTreeMap;

use sense_model::{
    ByteOffset, CompletionRequest, ContextEpoch, Generation, RawBytes, RequestId, SessionId,
    TerminalDimensions, TextRange, TriggerKind,
};

#[test]
fn raw_bytes_do_not_require_utf8() {
    let bytes = RawBytes::from(vec![b'f', 0x80, b'o']);
    assert_eq!(bytes.as_slice(), &[b'f', 0x80, b'o']);
    assert!(bytes.display_lossy().contains('\u{fffd}'));
}

#[test]
fn text_ranges_are_half_open_and_bounded() {
    assert!(TextRange::new(1, 3).is_valid_for(b"abcd"));
    assert!(TextRange::new(4, 4).is_valid_for(b"abcd"));
    assert!(!TextRange::new(3, 2).is_valid_for(b"abcd"));
    assert!(!TextRange::new(0, 5).is_valid_for(b"abcd"));
}

#[test]
fn request_validates_cursor_against_raw_buffer() {
    let request = CompletionRequest {
        session_id: SessionId::new(),
        request_id: RequestId(1),
        generation: Generation(1),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from("cd dotf"),
        cursor: ByteOffset(7),
        cwd: RawBytes::from("/tmp"),
        keymap: "viins".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::default(),
    };
    assert!(request.cursor_is_valid());
}
