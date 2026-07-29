use sense_model::{
    ByteOffset, CompletionItem, CompletionKind, CompletionResource, NativeShell, RawBytes,
    TextEdit, TextRange,
};
use sense_present::{FileIconPolicy, completion_icon};

fn item(kind: CompletionKind, path: Option<&str>) -> CompletionItem {
    let mut item = CompletionItem::native(
        "item",
        NativeShell::Zsh,
        path.unwrap_or("candidate"),
        TextEdit {
            range: TextRange {
                start: ByteOffset(0),
                end: ByteOffset(0),
            },
            new_text: RawBytes::default(),
        },
        "fingerprint",
    );
    item.kind = kind;
    item.resource = path.map(|path| CompletionResource::FileSystemPath { path: path.into() });
    item
}

#[test]
fn filetype_mode_uses_known_extensions() {
    let rust = item(CompletionKind::File, Some("src/main.rs"));
    assert_eq!(completion_icon(&rust, FileIconPolicy::Filetype), '');
    assert_eq!(completion_icon(&rust, FileIconPolicy::Generic), '󰈔');
}

#[test]
fn extensionless_paths_have_a_deterministic_generic_icon() {
    let extensionless = item(CompletionKind::File, Some("Makefile"));
    assert_eq!(
        completion_icon(&extensionless, FileIconPolicy::Filetype),
        '󰈔'
    );
}

#[test]
fn native_directory_kind_always_wins_over_the_path_suffix() {
    let directory = item(CompletionKind::Directory, Some("misleading.rs"));
    assert_eq!(completion_icon(&directory, FileIconPolicy::Filetype), '󰉋');
}

#[test]
fn semantic_kinds_have_stable_icons() {
    assert_eq!(
        completion_icon(
            &item(CompletionKind::Option, None),
            FileIconPolicy::Filetype
        ),
        '󰌋'
    );
    assert_eq!(
        completion_icon(
            &item(CompletionKind::Command, None),
            FileIconPolicy::Filetype,
        ),
        '󰆍'
    );
}
