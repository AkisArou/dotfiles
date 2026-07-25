use semver::Version;
use sense_model::SourceId;
use sense_provider_api::{
    Authority, DeadlinePolicy, PROVIDER_API_VERSION, ProviderCapabilities, ProviderClass,
    ProviderDescriptor, ProviderSelector,
};

fn descriptor() -> ProviderDescriptor {
    ProviderDescriptor {
        id: SourceId("git".into()),
        display_name: "Git".into(),
        version: Version::new(1, 0, 0),
        api_version: PROVIDER_API_VERSION,
        class: ProviderClass::ContextAdapter,
        capabilities: ProviderCapabilities::ENRICH | ProviderCapabilities::PREVIEW,
        authority: Authority::Authoritative,
        selectors: ProviderSelector {
            command_paths: vec![vec!["git".into()]],
            contexts: Vec::new(),
        },
        deadlines: DeadlinePolicy {
            soft_ms: 30,
            hard_ms: 200,
        },
        maximum_concurrency: 2,
        maximum_candidates: 10_000,
        cancellation: true,
        side_effect_free: true,
    }
}

#[test]
fn valid_descriptor_is_accepted() {
    descriptor().validate().unwrap();
}

#[test]
fn invalid_deadline_is_rejected() {
    let mut descriptor = descriptor();
    descriptor.deadlines.soft_ms = 300;
    assert!(descriptor.validate().is_err());
}
