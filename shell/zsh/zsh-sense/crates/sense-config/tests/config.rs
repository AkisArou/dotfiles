use sense_config::{ActivationMode, BorderStyle, Config, ConfigError, KeyAction};

#[test]
fn defaults_are_continuous_and_tab_is_manual_fallback() {
    let config = Config::default();
    assert_eq!(config.activation.mode, ActivationMode::Continuous);
    assert_eq!(config.keybindings.closed["tab"], KeyAction::Trigger);
    assert_eq!(config.keybindings.closed["ctrl-c"], KeyAction::Interrupt);
    assert_eq!(config.keybindings.popup["tab"], KeyAction::Accept);
    assert_eq!(config.keybindings.popup["ctrl-c"], KeyAction::Interrupt);
    assert_eq!(config.styles.menu, "fg=#bbbbbb,bg=#202020");
    assert_eq!(config.styles.selected, "bg=#343b41");
    assert_eq!(config.styles.label_match, "fg=#18a2fe,bold");
    assert_eq!(config.popup.border, BorderStyle::None);
    assert_eq!(config.popup.scrollbar_character, "▐");
    assert!(config.indicators.selected_marker.is_empty());
    assert_eq!(config.sources.zsh.fuzzy_min_query_chars, 3);
    config.validate().unwrap();
}

#[test]
fn scrollbar_character_must_be_one_printable_cell() {
    for value in ["", "wide", "界", "\n"] {
        let source = format!("version = 1\n[popup]\nscrollbar_character = {value:?}\n");
        assert!(matches!(
            Config::from_toml(&source),
            Err(ConfigError::Validation(_))
        ));
    }
    Config::from_toml("version = 1\n[popup]\nscrollbar_character = \"▕\"\n").unwrap();
}

#[test]
fn zsh_candidate_broadening_threshold_is_configurable() {
    let config = Config::from_toml(
        r"
        version = 1
        [sources.zsh]
        fuzzy_min_query_chars = 5
        ",
    )
    .unwrap();
    assert_eq!(config.sources.zsh.fuzzy_min_query_chars, 5);
}

#[test]
fn partial_toml_layers_over_defaults() {
    let config = Config::from_toml(
        r#"
        version = 1
        [activation]
        mode = "manual"
        debounce_ms = 12
        "#,
    )
    .unwrap();
    assert_eq!(config.activation.mode, ActivationMode::Manual);
    assert_eq!(config.activation.debounce_ms, 12);
    assert!(config.popup.enabled);
}

#[test]
fn unknown_fields_are_rejected() {
    let result = Config::from_toml(
        r"
        version = 1
        mystery = true
        ",
    );
    assert!(matches!(result, Err(ConfigError::Extract(_))));
}

#[test]
fn invalid_cross_field_values_are_reported() {
    let result = Config::from_toml(
        r"
        version = 1
        [activation]
        debounce_ms = 100
        max_debounce_ms = 10
        ",
    );
    assert!(matches!(result, Err(ConfigError::Validation(_))));
}

#[test]
fn popup_styles_must_be_single_zle_highlight_fields() {
    let result = Config::from_toml(
        r#"
        version = 1
        [styles]
        detail = "fg=#bbbbbb bold"
        "#,
    );
    assert!(matches!(result, Err(ConfigError::Validation(_))));
}

#[test]
fn popup_kind_styles_reject_unknown_kinds() {
    let result = Config::from_toml(
        r#"
        version = 1
        [styles.kinds]
        mystery = "fg=red"
        "#,
    );
    assert!(matches!(result, Err(ConfigError::Validation(_))));
}

#[test]
fn generated_schema_mentions_primary_sections() {
    let schema = Config::schema_json_pretty().unwrap();
    assert!(schema.contains("activation"));
    assert!(schema.contains("keybindings"));
    assert!(schema.contains("adapters"));
}

#[test]
fn external_adapter_configuration_is_typed_but_extensible() {
    let config = Config::from_toml(
        r#"
        version = 1
        [adapters.external.example]
        enabled = true
        manifest = "/opt/example/adapter.json"

        [adapters.external.example.configuration]
        endpoint = "local"
        limit = 12
        "#,
    )
    .unwrap();
    let adapter = &config.adapters.external["example"];
    assert_eq!(
        adapter.manifest.as_deref(),
        Some("/opt/example/adapter.json")
    );
    assert_eq!(adapter.configuration["limit"], 12);
}

#[test]
fn selected_profile_layers_before_the_main_file() {
    let temporary = tempfile::tempdir().unwrap();
    let profile_dir = temporary.path().join("profiles");
    std::fs::create_dir(&profile_dir).unwrap();
    std::fs::write(
        profile_dir.join("compact.toml"),
        "[popup]\nmax_rows = 4\ndecorations = \"minimal\"\n",
    )
    .unwrap();
    let config_path = temporary.path().join("config.toml");
    std::fs::write(
        &config_path,
        "profile = \"compact\"\n[popup]\nmax_rows = 7\n",
    )
    .unwrap();

    let config = Config::load(Some(&config_path)).unwrap();
    assert_eq!(config.profile, "compact");
    assert_eq!(config.popup.max_rows, 7);
    assert_eq!(
        config.popup.decorations,
        sense_config::DecorationMode::Minimal
    );
}

#[test]
fn unsafe_profile_names_are_rejected() {
    let result = Config::load_with_profile(None, Some("../escape"));
    assert!(matches!(result, Err(ConfigError::InvalidProfile(_))));
}

#[test]
fn checked_in_example_matches_the_generated_model() {
    let example = include_str!("../../../config.example.toml");
    Config::from_toml(example).unwrap();
}
