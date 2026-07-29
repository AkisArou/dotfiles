use std::collections::BTreeMap;

use sense_model::{
    ByteOffset, CompletionKind, CompletionRequest, ContextEpoch, Generation, InsertStrategy,
    RawBytes, RequestId, SessionId, TerminalDimensions, TextRange, TriggerKind,
};
use sense_shell_worker::{
    CaptureError, CaptureLimits, CaptureStore, CapturedGroup, CapturedMatch, ZshInsertionMetadata,
    ZshMatchFlags,
};

fn request(generation: u64) -> CompletionRequest {
    CompletionRequest {
        session_id: SessionId::new(),
        request_id: RequestId(7),
        generation: Generation(generation),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from("systemctl rstart"),
        cursor: ByteOffset(16),
        cwd: RawBytes::from("/tmp"),
        keymap: "emacs".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::new(),
    }
}

fn captured(insertion: impl Into<RawBytes>) -> CapturedMatch {
    CapturedMatch {
        insertion: insertion.into(),
        display: None,
        description: Some("Restart one or more units".into()),
        explanation: Some("systemd subcommand".into()),
        group: Some(CapturedGroup {
            name: "subcommands".into(),
            description: Some("systemctl command".into()),
            order: 1,
        }),
        replace_range: TextRange::new(10, 16),
        kind: CompletionKind::Subcommand,
        resource_path: None,
        flags: ZshMatchFlags::NO_SPACE,
        insertion_metadata: ZshInsertionMetadata {
            suffix: RawBytes::from(" "),
            ..ZshInsertionMetadata::default()
        },
        acceptance_identity: RawBytes::from("acceptance-record-1"),
        original_order: 0,
    }
}

#[test]
fn capture_preserves_raw_insertion_and_routes_acceptance() {
    let request = request(3);
    let mut store = CaptureStore::new(CaptureLimits::default()).expect("valid limits");
    let outcome = store
        .install(&request, vec![captured(vec![b'r', b'e', 0xff, b't'])])
        .expect("capture");

    let item = &outcome.batch.items[0];
    assert_eq!(item.edit.new_text.as_slice(), &[b'r', b'e', 0xff, b't']);
    assert!(!item.label.contains('\u{1b}'));
    let InsertStrategy::NativeMatch {
        shell: sense_model::NativeShell::Zsh,
        fingerprint,
    } = &item.insertion
    else {
        panic!("expected Zsh insertion identity");
    };
    let route = store
        .acceptance(request.request_id, request.generation, fingerprint)
        .expect("live route");
    assert_eq!(route.acceptance_identity.as_slice(), b"acceptance-record-1");
    assert_eq!(route.insertion_metadata.suffix.as_slice(), b" ");
}

#[test]
fn display_controls_are_escaped_before_reaching_the_ui() {
    let request = request(1);
    let mut candidate = captured("restart");
    candidate.display = Some("\u{1b}[31mrestart\n".into());
    let mut store = CaptureStore::new(CaptureLimits::default()).expect("valid limits");
    let outcome = store.install(&request, vec![candidate]).expect("capture");

    assert_eq!(outcome.batch.items[0].label, "\\u{1b}[31mrestart\\n");
}

#[test]
fn limits_truncate_the_tail_and_mark_the_batch_incomplete() {
    let request = request(1);
    let limits = CaptureLimits {
        max_candidates: 1,
        max_bytes: 1_024,
        max_ui_bytes_per_field: 128,
    };
    let mut store = CaptureStore::new(limits).expect("valid limits");
    let outcome = store
        .install(
            &request,
            vec![captured("restart"), captured("reset-failed")],
        )
        .expect("capture");

    assert_eq!(outcome.captured, 1);
    assert_eq!(outcome.dropped, 1);
    assert!(outcome.batch.is_incomplete);
}

#[test]
fn a_new_generation_invalidates_old_acceptance_fingerprints() {
    let first = request(1);
    let mut second = request(2);
    second.session_id = first.session_id;
    let mut store = CaptureStore::new(CaptureLimits::default()).expect("valid limits");
    let first_outcome = store
        .install(&first, vec![captured("restart")])
        .expect("capture");
    let InsertStrategy::NativeMatch {
        shell: sense_model::NativeShell::Zsh,
        fingerprint,
    } = &first_outcome.batch.items[0].insertion
    else {
        panic!("expected fingerprint");
    };
    let stable_id = first_outcome.batch.items[0].id.clone();

    let second_outcome = store
        .install(&second, vec![captured("restart")])
        .expect("capture");

    assert_eq!(stable_id, second_outcome.batch.items[0].id);
    assert!(matches!(
        store.acceptance(first.request_id, first.generation, fingerprint),
        Err(CaptureError::StaleCapture { .. })
    ));
}

#[test]
fn invalid_edit_ranges_reject_the_capture() {
    let request = request(1);
    let mut candidate = captured("restart");
    candidate.replace_range = TextRange::new(10, 100);
    let mut store = CaptureStore::new(CaptureLimits::default()).expect("valid limits");

    assert_eq!(
        store.install(&request, vec![candidate]),
        Err(CaptureError::InvalidRange { index: 0 })
    );
}
