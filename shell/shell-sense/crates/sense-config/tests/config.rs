use sense_config::{ActivationMode, BorderStyle, Config, ConfigError, FileIconMode, KeyAction};

#[test]
fn defaults_are_continuous_and_tab_is_manual_fallback() {
    let config = Config::default();
    assert_eq!(config.activation.mode, ActivationMode::Continuous);
    assert_eq!(config.keybindings.closed["tab"], KeyAction::Trigger);
    assert_eq!(config.keybindings.closed["ctrl-c"], KeyAction::Interrupt);
    assert_eq!(config.keybindings.popup["tab"], KeyAction::Accept);
    assert_eq!(config.keybindings.popup["ctrl-c"], KeyAction::Interrupt);
    assert_eq!(
        config.keybindings.popup["ctrl-f"],
        KeyAction::DocumentationPageDown
    );
    assert_eq!(
        config.keybindings.popup["ctrl-b"],
        KeyAction::DocumentationPageUp
    );
    assert_eq!(
        config.keybindings.popup["ctrl-g"],
        KeyAction::ToggleDocumentation
    );
    assert_eq!(config.styles.menu, "fg=#bbbbbb,bg=#202020");
    assert_eq!(config.styles.selected, "bg=#343b41");
    assert_eq!(config.styles.label_match, "fg=#18a2fe,bold");
    assert_eq!(config.popup.border, BorderStyle::None);
    assert_eq!(config.popup.scrollbar_character, "▐");
    assert_eq!(config.popup.scrolloff, 2);
    assert!(config.popup.cycle);
    assert_eq!(config.documentation.padding, 0);
    assert!(config.documentation.scrollbar);
    assert_eq!(config.indicators.file_icons, FileIconMode::Filetype);
    assert!(config.indicators.selected_marker.is_empty());
    assert_eq!(config.sources.zsh.fuzzy_min_query_chars, 3);
    assert_eq!(config.sources.fish.fuzzy_min_query_chars, 3);
    assert_eq!(config.sources.bash.fuzzy_min_query_chars, 3);
    config.validate().unwrap();
}

#[test]
fn popup_scrolloff_must_fit_inside_the_viewport() {
    let result = Config::from_toml("version = 4\n[popup]\nmax_rows = 4\nscrolloff = 4\n");
    assert!(matches!(result, Err(ConfigError::Validation(_))));

    let config = Config::from_toml("version = 4\n[popup]\nmax_rows = 4\nscrolloff = 2\n").unwrap();
    assert_eq!(config.popup.scrolloff, 2);
}

#[test]
fn popup_reserves_a_refresh_gutter() {
    let result = Config::from_toml("version = 4\n[popup]\npadding = 0\n");
    assert!(matches!(result, Err(ConfigError::Validation(_))));

    Config::from_toml("version = 4\n[popup]\npadding = 1\n").unwrap();
}

#[test]
fn popup_cycling_can_be_disabled() {
    let config = Config::from_toml("version = 4\n[popup]\ncycle = false\n").unwrap();
    assert!(!config.popup.cycle);
}

#[test]
fn scrollbar_character_must_be_one_printable_cell() {
    for value in ["", "wide", "界", "\n"] {
        let source = format!("version = 4\n[popup]\nscrollbar_character = {value:?}\n");
        assert!(matches!(
            Config::from_toml(&source),
            Err(ConfigError::Validation(_))
        ));
    }
    Config::from_toml("version = 4\n[popup]\nscrollbar_character = \"▕\"\n").unwrap();
}

#[test]
fn zsh_candidate_broadening_threshold_is_configurable() {
    let config = Config::from_toml(
        r"
        version = 4
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
        version = 4
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
        version = 4
        mystery = true
        ",
    );
    assert!(matches!(result, Err(ConfigError::Extract(_))));
}

#[test]
fn removed_product_scopes_are_not_accepted() {
    for section in ["diagnostics", "snippets", "history", "safety", "native"] {
        let source = format!("version = 4\n[{section}]\nenabled = true\n");
        assert!(matches!(
            Config::from_toml(&source),
            Err(ConfigError::Extract(_))
        ));
    }
}

#[test]
fn removed_no_op_fields_are_rejected() {
    let result = Config::from_toml(
        r"
        version = 4
        [activation]
        max_debounce_ms = 10
        ",
    );
    assert!(matches!(result, Err(ConfigError::Extract(_))));
}

#[test]
fn popup_styles_must_be_single_zle_highlight_fields() {
    let result = Config::from_toml(
        r#"
        version = 4
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
        version = 4
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
fn documentation_resolvers_use_typed_kinds_and_complete_argument_placeholders() {
    let config = Config::from_toml(
        r#"
        version = 4
        [adapters.documentation]
        resolvers = [
          { name = "metadata", kinds = ["file"], command = ["file", "--", "$value"] }
        ]
        "#,
    )
    .unwrap();
    assert_eq!(config.adapters.documentation.resolvers.len(), 1);

    let invalid = Config::from_toml(
        r#"
        version = 4
        [adapters.documentation]
        resolvers = [
          { name = "bad", kinds = ["file"], command = ["file", "prefix-$value"] }
        ]
        "#,
    );
    assert!(matches!(invalid, Err(ConfigError::Validation(_))));
}

#[test]
fn unimplemented_external_adapter_configuration_is_rejected() {
    let result = Config::from_toml(
        r"
        version = 4
        [adapters.external.example]
        enabled = true
        ",
    );
    assert!(matches!(result, Err(ConfigError::Extract(_))));
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
