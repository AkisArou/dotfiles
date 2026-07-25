use std::env;
#[cfg(feature = "native-bindings")]
use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[cfg(feature = "native-bindings")]
use sense_zsh_abi::{TargetIdentity, ZshIdentity, probe_for_target};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo::rerun-if-env-changed=SENSE_ZSH_EXECUTABLE");
    println!("cargo::rerun-if-env-changed=SENSE_ZSH_SOURCE");
    println!("cargo::rerun-if-env-changed=SENSE_ZSH_BUILD");

    let output = PathBuf::from(required_env("OUT_DIR")?);
    #[cfg(feature = "native-bindings")]
    {
        let executable =
            env::var_os("SENSE_ZSH_EXECUTABLE").unwrap_or_else(|| OsString::from("zsh"));
        let target = TargetIdentity {
            triple: required_env("TARGET")?,
            pointer_width: required_env("CARGO_CFG_TARGET_POINTER_WIDTH")?,
            endian: required_env("CARGO_CFG_TARGET_ENDIAN")?,
        };
        let identity = probe_for_target(&executable, target)?;
        write_identity(&output.join("identity.rs"), Some(&identity))?;
        generate_bindings(&output)?;
    }
    #[cfg(not(feature = "native-bindings"))]
    write_identity(&output.join("identity.rs"), None)?;
    Ok(())
}

fn required_env(name: &str) -> Result<String, io::Error> {
    env::var(name).map_err(|error| io::Error::other(format!("{name} is unavailable: {error}")))
}

#[cfg(feature = "native-bindings")]
fn write_identity(path: &Path, identity: Option<&ZshIdentity>) -> Result<(), io::Error> {
    let identity = identity.ok_or_else(|| io::Error::other("native identity is required"))?;
    let contents = format!(
        "pub const BUILD_IDENTITY: Option<BuildIdentity> = Some(BuildIdentity {{\n  executable: {:?},\n  version: {:?},\n  patchlevel: {:?},\n  module_suffix: {:?},\n  dynamic_modules: {},\n  executable_digest: {:?},\n  target: {:?},\n  pointer_width: {:?},\n  endian: {:?},\n  native_abi_revision: {},\n  abi_key: {:?},\n}});\n",
        identity.executable.to_string_lossy(),
        identity.version,
        identity.patchlevel,
        identity.module_suffix,
        identity.dynamic_modules,
        identity.executable_digest,
        identity.target.triple,
        identity.target.pointer_width,
        identity.target.endian,
        sense_zsh_abi::NATIVE_ABI_REVISION,
        identity.abi_key(),
    );
    fs::write(path, contents)
}

#[cfg(not(feature = "native-bindings"))]
fn write_identity(
    path: &Path,
    _identity: Option<&sense_zsh_abi::ZshIdentity>,
) -> Result<(), io::Error> {
    fs::write(
        path,
        "pub const BUILD_IDENTITY: Option<BuildIdentity> = None;\n",
    )
}

#[cfg(feature = "native-bindings")]
fn generate_bindings(output: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let source = PathBuf::from(required_env("SENSE_ZSH_SOURCE")?);
    let build = env::var_os("SENSE_ZSH_BUILD").map_or_else(|| source.clone(), PathBuf::from);
    validate_build_tree(&source, &build)?;

    let wrapper = output.join("wrapper.h");
    fs::write(
        &wrapper,
        "#include \"Src/zsh.mdh\"\n#include \"Src/Zle/zle.mdh\"\n#include \"Src/Zle/complete.mdh\"\n",
    )?;
    let bindings = bindgen::Builder::default()
        .header(wrapper.to_string_lossy())
        .clang_arg(format!("-I{}", source.display()))
        .clang_arg(format!("-I{}", build.display()))
        .clang_arg("-DMODULE")
        .allowlist_recursively(true)
        .allowlist_type("(module|builtin|features|cmatch|cmgroup|cadata)")
        .allowlist_type("(Module|Builtin|Features|Cmatch|Cmgroup|Cadata)")
        .allowlist_function("(featuresarray|handlefeatures|setfeatureenables|freearray)")
        .allowlist_function("(addhookfunc|deletehookfunc|unmetafy|metafy)")
        .allowlist_var("(CAF|CMF|GF)_.*")
        .derive_default(true)
        .wrap_unsafe_ops(true)
        .generate()
        .map_err(io::Error::other)?;
    bindings.write_to_file(output.join("bindings.rs"))?;
    Ok(())
}

#[cfg(feature = "native-bindings")]
fn validate_build_tree(source: &Path, build: &Path) -> Result<(), io::Error> {
    for required in [
        source.join("Src/zsh.h"),
        source.join("Src/Zle/comp.h"),
        build.join("config.h"),
        build.join("Src/zsh.mdh"),
        build.join("Src/Zle/zle.mdh"),
        build.join("Src/Zle/complete.mdh"),
    ] {
        if !required.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!(
                    "configured Zsh build artifact is missing: {}",
                    required.display()
                ),
            ));
        }
    }
    Ok(())
}
