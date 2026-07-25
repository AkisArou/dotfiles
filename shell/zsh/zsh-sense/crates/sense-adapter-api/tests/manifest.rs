use std::collections::BTreeMap;

use semver::Version;
use sense_adapter_api::{
    ADAPTER_MANIFEST_VERSION, AdapterManifest, AdapterOrigin, AdapterPermissions,
    AdapterProtocolVersion, SideEffectDeclaration,
};
use sense_model::SourceId;
use sense_provider_api::{
    Authority, DeadlinePolicy, PROVIDER_API_VERSION, ProviderCapabilities, ProviderClass,
    ProviderDescriptor, ProviderSelector,
};

fn manifest() -> AdapterManifest {
    AdapterManifest {
        manifest_version: ADAPTER_MANIFEST_VERSION,
        protocol: AdapterProtocolVersion::CURRENT,
        provider: ProviderDescriptor {
            id: SourceId("example".into()),
            display_name: "Example".into(),
            version: Version::new(1, 2, 3),
            api_version: PROVIDER_API_VERSION,
            class: ProviderClass::ContextAdapter,
            capabilities: ProviderCapabilities::COMPLETE,
            authority: Authority::Advisory,
            selectors: ProviderSelector {
                command_paths: Vec::new(),
                contexts: Vec::new(),
            },
            deadlines: DeadlinePolicy {
                soft_ms: 50,
                hard_ms: 200,
            },
            maximum_concurrency: 1,
            maximum_candidates: 1_000,
            cancellation: true,
            side_effect_free: true,
        },
        command: vec!["sense-example-adapter".into()],
        origin: AdapterOrigin::Installed,
        permissions: AdapterPermissions {
            environment_allowlist: Vec::new(),
            network: false,
            side_effects: SideEffectDeclaration::None,
        },
        configuration_schema: None,
        default_configuration: BTreeMap::new(),
        maximum_output_bytes: 4 * 1024 * 1024,
    }
}

#[test]
fn bounded_manifest_is_valid() {
    manifest().validate().unwrap();
}

#[test]
fn empty_executable_is_rejected() {
    let mut manifest = manifest();
    manifest.command = vec![String::new()];
    assert!(manifest.validate().is_err());
}
