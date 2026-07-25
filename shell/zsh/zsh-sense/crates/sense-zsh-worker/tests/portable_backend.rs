use std::path::Path;
use std::process::Command;

#[test]
fn portable_backend_captures_descriptions_groups_and_acceptance() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("worker crate is inside the workspace");
    let output = Command::new("zsh")
        .arg(workspace.join("tests/portable-capture.zsh"))
        .output()
        .expect("zsh must be installed to test the Zsh worker");

    assert!(
        output.status.success(),
        "portable capture failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.stdout, b"portable-capture-ok\n");
}
