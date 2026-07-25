//! Greenfield native Zsh module.
//!
//! The module stays single-threaded and contains only Zsh-facing capture and
//! selection operations. All ranking, parsing, caching, I/O, and adapters live
//! outside the Zsh process.

pub const MODULE_ABI_REVISION: u16 = sense_zsh_sys::NATIVE_ABI_REVISION;

#[cfg(feature = "native-module")]
mod entrypoints {
    use std::ffi::{c_char, c_int, c_void};
    use std::panic::{AssertUnwindSafe, catch_unwind};

    fn ffi_boundary(operation: impl FnOnce() -> c_int) -> c_int {
        catch_unwind(AssertUnwindSafe(operation)).unwrap_or(1)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn setup_(_module: *mut c_void) -> c_int {
        ffi_boundary(|| {
            sense_zsh_sys::BUILD_IDENTITY.map_or(1, |identity| {
                i32::from(!identity.dynamic_modules || identity.native_abi_revision == 0)
            })
        })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn features_(
        module: *mut c_void,
        features: *mut *mut *mut c_char,
    ) -> c_int {
        ffi_boundary(|| unsafe { sense_zsh_sys::module::report_empty_features(module, features) })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn enables_(module: *mut c_void, enables: *mut *mut c_int) -> c_int {
        ffi_boundary(|| unsafe { sense_zsh_sys::module::handle_empty_features(module, enables) })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn boot_(_module: *mut c_void) -> c_int {
        ffi_boundary(|| 0)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn cleanup_(module: *mut c_void) -> c_int {
        ffi_boundary(|| unsafe { sense_zsh_sys::module::disable_empty_features(module) })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn finish_(_module: *mut c_void) -> c_int {
        ffi_boundary(|| 0)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn native_abi_revision_is_nonzero() {
        assert_ne!(super::MODULE_ABI_REVISION, 0);
    }
}
