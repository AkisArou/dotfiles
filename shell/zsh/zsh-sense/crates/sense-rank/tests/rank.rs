use sense_config::{MatchingConfig, TypoMode};
use sense_model::{
    CompletionItem, CompletionKind, DocumentationState, GroupId, InsertStrategy, MarkupContent,
    MarkupKind, RawBytes, TextEdit, TextRange,
};
use sense_rank::{RankConfig, RankSignals, Ranker};

fn item(id: &str, label: &str, order: u32) -> CompletionItem {
    let mut item =
        CompletionItem::plain(id, "zsh", label, TextEdit::new(TextRange::new(0, 0), label));
    item.original_order = order;
    item
}

#[test]
fn typo_matching_finds_restart_for_rstart() {
    let ranked = Ranker::new(RankConfig::default()).rank(
        "rstart",
        vec![item("reload", "reload", 0), item("restart", "restart", 1)],
        None,
        &RankSignals::default(),
    );

    assert_eq!(ranked.items[0].label, "restart");
    assert!(
        !ranked.items[0]
            .match_result
            .as_ref()
            .expect("matched")
            .indices
            .is_empty()
    );
}

#[test]
fn fuzzy_matching_finds_dotfiles_for_dfil() {
    let ranked = Ranker::new(RankConfig::default()).rank(
        "dfil",
        vec![
            item("downloads", "Downloads", 0),
            item("dotfiles", "dotfiles", 1),
        ],
        None,
        &RankSignals::default(),
    );

    assert_eq!(ranked.items[0].label, "dotfiles");
}

#[test]
fn short_queries_do_not_enable_adaptive_typos() {
    let matching = MatchingConfig {
        typos: TypoMode::Adaptive,
        typo_min_query_chars: 4,
        ..MatchingConfig::default()
    };
    let ranked = Ranker::new(RankConfig::from_matching(&matching)).rank(
        "ab",
        vec![item("ac", "ac", 0)],
        None,
        &RankSignals::default(),
    );

    assert!(ranked.items.is_empty());
}

#[test]
fn ranking_preserves_group_order() {
    let mut branch_main = item("branch-main", "main", 0);
    branch_main.group = Some(GroupId("branches".into()));
    let mut branch_feature = item("branch-feature", "feature/main", 1);
    branch_feature.group = Some(GroupId("branches".into()));
    let mut file = item("file-main", "main.rs", 0);
    file.group = Some(GroupId("files".into()));

    let ranked = Ranker::new(RankConfig::default()).rank(
        "main",
        vec![branch_feature, branch_main, file],
        None,
        &RankSignals::default(),
    );

    assert_eq!(ranked.items[0].id.0, "branch-main");
    assert_eq!(ranked.items[1].id.0, "branch-feature");
    assert_eq!(ranked.items[2].id.0, "file-main");
}

#[test]
fn duplicate_enrichment_keeps_zsh_insertion_identity() {
    let mut zsh = item("zsh-restart", "restart", 5);
    zsh.insertion = InsertStrategy::ZshMatch {
        fingerprint: RawBytes::from("opaque-zsh-match"),
    };
    let mut adapter = item("systemd-restart", "restart", 0);
    adapter.source.0 = "systemd".into();
    adapter.kind = CompletionKind::Subcommand;
    adapter.detail = Some("Restart one or more units".into());
    adapter.documentation = DocumentationState::Resolved(MarkupContent {
        kind: MarkupKind::PlainText,
        value: "Stops and then starts the selected units.".into(),
    });

    let ranked = Ranker::new(RankConfig::default()).rank(
        "restart",
        vec![adapter, zsh],
        None,
        &RankSignals::default(),
    );

    assert_eq!(ranked.duplicates_merged, 1);
    assert_eq!(ranked.items.len(), 1);
    assert!(matches!(
        ranked.items[0].insertion,
        InsertStrategy::ZshMatch { .. }
    ));
    assert_eq!(ranked.items[0].kind, CompletionKind::Subcommand);
    assert_eq!(
        ranked.items[0].detail.as_deref(),
        Some("Restart one or more units")
    );
    assert!(matches!(
        ranked.items[0].documentation,
        DocumentationState::Resolved(_)
    ));
}

#[test]
fn selection_stays_on_the_same_item_after_refilter() {
    let selected = item("restart", "restart", 1).id;
    let ranked = Ranker::new(RankConfig::default()).rank(
        "re",
        vec![item("reload", "reload", 0), item("restart", "restart", 1)],
        Some(&selected),
        &RankSignals::default(),
    );

    assert_eq!(
        ranked.items[ranked.selected_index.expect("selected")].id,
        selected
    );
}

#[test]
fn empty_query_keeps_original_order_when_signals_are_equal() {
    let ranked = Ranker::new(RankConfig::default()).rank(
        "",
        vec![item("second", "second", 2), item("first", "first", 1)],
        None,
        &RankSignals::default(),
    );

    assert_eq!(ranked.items[0].id.0, "first");
    assert_eq!(ranked.items[1].id.0, "second");
}
