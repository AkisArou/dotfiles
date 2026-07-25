//! Narrow, build-specific raw-binding boundary for the native Zsh module.
//!
//! Enabling native bindings requires an explicitly selected, configured Zsh
//! source/build tree. No daemon or portable component depends on these types.

pub use sense_zsh_abi::{NATIVE_ABI_REVISION, TargetIdentity, ZshIdentity};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuildIdentity {
    pub executable: &'static str,
    pub version: &'static str,
    pub patchlevel: &'static str,
    pub module_suffix: &'static str,
    pub dynamic_modules: bool,
    pub executable_digest: &'static str,
    pub target: &'static str,
    pub pointer_width: &'static str,
    pub endian: &'static str,
    pub native_abi_revision: u16,
    pub abi_key: &'static str,
}

include!(concat!(env!("OUT_DIR"), "/identity.rs"));

#[cfg(feature = "native-bindings")]
#[allow(
    dead_code,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unsafe_code
)]
mod raw {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

#[cfg(feature = "native-bindings")]
pub mod module {
    //! Narrow operations used by the native module's standard entry points.

    use std::ffi::{c_char, c_int, c_void};

    use super::raw;

    /// Return Zsh's feature array for a module with no exposed builtins yet.
    ///
    /// # Safety
    ///
    /// `module` and `output` must be the live pointers supplied by Zsh to the
    /// module's `features_` entry point.
    pub unsafe fn report_empty_features(
        module: *mut c_void,
        output: *mut *mut *mut c_char,
    ) -> c_int {
        let mut features = raw::features::default();
        let array = unsafe { raw::featuresarray(module.cast(), &raw mut features) };
        unsafe { output.write(array) };
        0
    }

    /// Apply enable/disable requests for a module with no exposed features.
    ///
    /// # Safety
    ///
    /// Both pointers must be the live values supplied by Zsh to `enables_`.
    pub unsafe fn handle_empty_features(module: *mut c_void, enables: *mut *mut c_int) -> c_int {
        let mut features = raw::features::default();
        unsafe { raw::handlefeatures(module.cast(), &raw mut features, enables) }
    }

    /// Disable all features during module cleanup.
    ///
    /// # Safety
    ///
    /// `module` must be the live module pointer supplied by Zsh.
    pub unsafe fn disable_empty_features(module: *mut c_void) -> c_int {
        let mut features = raw::features::default();
        unsafe { raw::setfeatureenables(module.cast(), &raw mut features, std::ptr::null_mut()) }
    }
}
