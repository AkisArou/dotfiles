use semver::Version;
use sense_model::{
    ByteOffset, CompletionKind, CompletionRequest, Confidence, ContextEpoch, DocumentationState,
    Generation, ItemCapabilities, ItemId, ItemTags, NativeShell, RawBytes, RequestId, SessionId,
    TerminalDimensions, TextEdit, TextRange, TriggerKind,
};
use sense_provider_api::{
    AdapterCapabilities, AdapterSelector, Authority, ContextAdapterDescriptor, DeadlinePolicy,
    NativeCandidate, NativeProviderCapabilities, NativeProviderDescriptor, NativeQueryMode,
    PROVIDER_API_VERSION, broad_query,
};
use std::collections::BTreeMap;

fn native_descriptor() -> NativeProviderDescriptor {
    NativeProviderDescriptor {
        shell: NativeShell::Fish,
        version: Version::new(1, 0, 0),
        api_version: PROVIDER_API_VERSION,
        capabilities: NativeProviderCapabilities::DESCRIPTIONS
            | NativeProviderCapabilities::BROAD_QUERY,
        deadlines: DeadlinePolicy {
            soft_ms: 30,
            hard_ms: 200,
        },
        maximum_candidates: 10_000,
        cancellation: true,
    }
}

fn adapter_descriptor() -> ContextAdapterDescriptor {
    ContextAdapterDescriptor {
        id: sense_model::SourceId("git".into()),
        display_name: "Git".into(),
        version: Version::new(1, 0, 0),
        api_version: PROVIDER_API_VERSION,
        capabilities: AdapterCapabilities::ENRICH | AdapterCapabilities::RESOLVE,
        authority: Authority::Authoritative,
        selectors: AdapterSelector {
            command_paths: vec![vec!["git".into()]],
            contexts: Vec::new(),
        },
        deadlines: DeadlinePolicy {
            soft_ms: 30,
            hard_ms: 200,
        },
        maximum_concurrency: 2,
        maximum_enrichments: 10_000,
        cancellation: true,
        side_effect_free: true,
    }
}

#[test]
fn valid_descriptors_are_accepted() {
    native_descriptor().validate().unwrap();
    adapter_descriptor().validate().unwrap();
}

#[test]
fn invalid_deadline_is_rejected() {
    let mut descriptor = native_descriptor();
    descriptor.deadlines.soft_ms = 300;
    assert!(descriptor.validate().is_err());
}

#[test]
fn native_normalization_assigns_shell_authority() {
    let candidate = NativeCandidate {
        id: ItemId("fish:1".into()),
        label: "restart".into(),
        label_detail: None,
        filter_text: None,
        kind: CompletionKind::Subcommand,
        tags: ItemTags::empty(),
        detail: Some("Restart a service".into()),
        documentation: DocumentationState::None,
        group: None,
        edit: TextEdit::new(TextRange::new(10, 16), "restart"),
        acceptance: RawBytes::from("opaque"),
        original_order: 0,
        confidence: Confidence::Authoritative,
        capabilities: ItemCapabilities::empty(),
        resource: None,
        opaque_data: RawBytes::default(),
    };

    let item = candidate.into_completion_item(NativeShell::Fish);
    assert_eq!(item.source.0, "fish");
    assert!(matches!(
        item.insertion,
        sense_model::InsertStrategy::NativeMatch {
            shell: NativeShell::Fish,
            ..
        }
    ));
}

fn request(buffer: &str, cursor: u32) -> CompletionRequest {
    CompletionRequest {
        session_id: SessionId::new(),
        request_id: RequestId(1),
        generation: Generation(1),
        context_epoch: ContextEpoch::default(),
        buffer: RawBytes::from(buffer),
        cursor: ByteOffset(cursor),
        cwd: RawBytes::from("/tmp"),
        keymap: "default".into(),
        terminal: TerminalDimensions::default(),
        trigger: TriggerKind::Automatic,
        environment: BTreeMap::new(),
    }
}

#[test]
fn broad_query_preserves_native_structure() {
    for (buffer, range, expected) in [
        ("systemctl rstart", TextRange::new(10, 16), "systemctl "),
        ("ls --recusr", TextRange::new(3, 11), "ls --"),
        ("cd dotfiles/nv", TextRange::new(3, 14), "cd dotfiles/"),
        ("cmd --color=au", TextRange::new(4, 14), "cmd --color="),
    ] {
        let request = request(buffer, u32::try_from(buffer.len()).unwrap());
        let query = broad_query(&request, range).unwrap();
        assert_eq!(query.mode, NativeQueryMode::Broad);
        assert_eq!(query.buffer.display_lossy(), expected);
        assert_eq!(query.cursor.as_usize(), expected.len());
    }
}
