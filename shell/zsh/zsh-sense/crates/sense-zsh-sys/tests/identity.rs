use sense_zsh_sys::BUILD_IDENTITY;

#[cfg(not(feature = "native-bindings"))]
#[test]
fn portable_build_has_no_embedded_zsh_identity() {
    assert!(BUILD_IDENTITY.is_none());
}

#[cfg(feature = "native-bindings")]
#[test]
fn native_build_has_an_embedded_zsh_identity() {
    let identity = BUILD_IDENTITY.expect("native build identity");
    assert!(!identity.version.is_empty());
    assert_eq!(identity.abi_key.len(), 64);
}
