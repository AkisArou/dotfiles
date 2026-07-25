use std::env;
use std::io;

fn main() -> Result<(), io::Error> {
    for (cargo_name, rust_name) in [
        ("TARGET", "SENSE_BUILD_TARGET"),
        (
            "CARGO_CFG_TARGET_POINTER_WIDTH",
            "SENSE_BUILD_POINTER_WIDTH",
        ),
        ("CARGO_CFG_TARGET_ENDIAN", "SENSE_BUILD_ENDIAN"),
    ] {
        let value = env::var(cargo_name)
            .map_err(|error| io::Error::other(format!("{cargo_name} is unavailable: {error}")))?;
        println!("cargo::rustc-env={rust_name}={value}");
    }
    Ok(())
}
