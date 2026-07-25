use sense_zsh_abi::{TargetIdentity, compute_abi_key, probe};

#[test]
fn probe_produces_a_stable_build_specific_key() {
    let first = probe("zsh").unwrap();
    let second = probe(&first.executable).unwrap();
    assert_eq!(first, second);
    assert_eq!(first.abi_key(), compute_abi_key(&first));
    assert_eq!(first.target, TargetIdentity::build_target());
    assert!(first.dynamic_modules);
}
