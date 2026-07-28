//! Typed, validated configuration for zsh-sense.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use etcetera::{AppStrategy, AppStrategyArgs, choose_app_strategy};
use figment::Figment;
use figment::providers::{Env, Format, Serialized, Toml};
use schemars::{JsonSchema, schema_for};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use unicode_width::UnicodeWidthStr;

pub const CONFIG_VERSION: u32 = 1;

#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("could not determine the user configuration directory: {0}")]
    Directory(#[from] etcetera::HomeDirError),
    #[error("configuration could not be loaded: {0}")]
    Extract(#[source] Box<figment::Error>),
    #[error("configuration is invalid:\n{}", .0.join("\n"))]
    Validation(Vec<String>),
    #[error("configuration schema could not be encoded: {0}")]
    Schema(#[from] serde_json::Error),
    #[error("profile name is invalid: {0}")]
    InvalidProfile(String),
    #[error("selected profile {profile:?} does not exist at {path}")]
    ProfileNotFound { profile: String, path: PathBuf },
}

impl From<figment::Error> for ConfigError {
    fn from(error: figment::Error) -> Self {
        Self::Extract(Box::new(error))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfigPaths {
    pub config_file: PathBuf,
    pub profiles_dir: PathBuf,
    pub data_dir: PathBuf,
    pub cache_dir: PathBuf,
    pub state_dir: Option<PathBuf>,
    pub runtime_dir: Option<PathBuf>,
}

impl ConfigPaths {
    /// Discover platform-appropriate application paths.
    ///
    /// # Errors
    ///
    /// Returns an error when the platform's home/config directories cannot be
    /// determined.
    pub fn discover() -> Result<Self, ConfigError> {
        let strategy = choose_app_strategy(AppStrategyArgs {
            top_level_domain: String::new(),
            author: String::new(),
            app_name: "zsh-sense".into(),
        })?;
        let config_file = strategy.in_config_dir("config.toml");
        let profiles_dir = strategy.in_config_dir("profiles");
        Ok(Self {
            config_file,
            profiles_dir,
            data_dir: strategy.data_dir(),
            cache_dir: strategy.cache_dir(),
            state_dir: strategy.state_dir(),
            runtime_dir: strategy.runtime_dir(),
        })
    }

    #[must_use]
    pub fn socket_path(&self) -> PathBuf {
        self.runtime_dir
            .as_ref()
            .unwrap_or(&self.cache_dir)
            .join("daemon.sock")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    pub version: u32,
    pub profile: String,
    pub activation: ActivationConfig,
    pub keybindings: KeybindingsConfig,
    pub matching: MatchingConfig,
    pub popup: PopupConfig,
    pub indicators: IndicatorConfig,
    pub documentation: DocumentationConfig,
    pub ghost_text: GhostTextConfig,
    pub sources: SourcesConfig,
    pub adapters: AdaptersConfig,
    pub snippets: SnippetConfig,
    pub history: HistoryConfig,
    pub diagnostics: DiagnosticsConfig,
    pub safety: SafetyConfig,
    pub cache: CacheConfig,
    pub native: NativeConfig,
    pub logging: LoggingConfig,
    pub styles: StyleConfig,
    pub rules: Vec<RuleConfig>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: CONFIG_VERSION,
            profile: "default".into(),
            activation: ActivationConfig::default(),
            keybindings: KeybindingsConfig::default(),
            matching: MatchingConfig::default(),
            popup: PopupConfig::default(),
            indicators: IndicatorConfig::default(),
            documentation: DocumentationConfig::default(),
            ghost_text: GhostTextConfig::default(),
            sources: SourcesConfig::default(),
            adapters: AdaptersConfig::default(),
            snippets: SnippetConfig::default(),
            history: HistoryConfig::default(),
            diagnostics: DiagnosticsConfig::default(),
            safety: SafetyConfig::default(),
            cache: CacheConfig::default(),
            native: NativeConfig::default(),
            logging: LoggingConfig::default(),
            styles: StyleConfig::default(),
            rules: Vec::new(),
        }
    }
}

impl Config {
    /// Load defaults, an optional TOML file, and explicit environment values.
    ///
    /// # Errors
    ///
    /// Returns an error when paths cannot be discovered, configuration cannot
    /// be deserialized, unknown fields are present, or validation fails.
    pub fn load(path: Option<&Path>) -> Result<Self, ConfigError> {
        Self::load_with_profile(path, None)
    }

    /// Load configuration with an optional explicit profile selection.
    ///
    /// Profile files live next to the main file under `profiles/NAME.toml`.
    /// Their values layer between built-in defaults and the main user file.
    /// The explicit argument takes precedence over `ZSH_SENSE_PROFILE`, which
    /// takes precedence over the `profile` key in the main file.
    ///
    /// # Errors
    ///
    /// Returns an error for invalid/missing profiles and all errors documented
    /// by [`Config::load`].
    pub fn load_with_profile(
        path: Option<&Path>,
        explicit_profile: Option<&str>,
    ) -> Result<Self, ConfigError> {
        let discovered;
        let path = if let Some(path) = path {
            path
        } else {
            discovered = ConfigPaths::discover()?.config_file;
            &discovered
        };

        let selected_profile = select_profile(path, explicit_profile)?;
        validate_profile_name(&selected_profile)?;

        let mut figment = Figment::from(Serialized::defaults(Self::default()));
        if selected_profile != "default" {
            let profile_path = profile_path(path, &selected_profile);
            if !profile_path.is_file() {
                return Err(ConfigError::ProfileNotFound {
                    profile: selected_profile,
                    path: profile_path,
                });
            }
            figment = figment.merge(Toml::file(profile_path));
        }
        if path.exists() {
            figment = figment.merge(Toml::file(path));
        }
        figment = figment.merge(Env::prefixed("ZSH_SENSE_").split("__"));
        let mut config: Self = figment.extract()?;
        config.profile = selected_profile;
        config.validate()?;
        Ok(config)
    }

    /// Deserialize and validate a TOML fragment layered over defaults.
    ///
    /// # Errors
    ///
    /// Returns an error for malformed/unknown fields or failed validation.
    pub fn from_toml(source: &str) -> Result<Self, ConfigError> {
        let config: Self = Figment::from(Serialized::defaults(Self::default()))
            .merge(Toml::string(source))
            .extract()?;
        config.validate()?;
        Ok(config)
    }

    /// Validate cross-field invariants not expressible through deserialization.
    ///
    /// # Errors
    ///
    /// Returns all discovered validation issues in one error.
    pub fn validate(&self) -> Result<(), ConfigError> {
        let mut issues = Vec::new();
        if self.version != CONFIG_VERSION {
            issues.push(format!(
                "version must be {CONFIG_VERSION}, got {}",
                self.version
            ));
        }
        if self.activation.debounce_ms > self.activation.max_debounce_ms {
            issues.push("activation.debounce_ms must not exceed max_debounce_ms".into());
        }
        if self.popup.enabled && self.popup.max_rows == 0 {
            issues.push("popup.max_rows must be at least 1 when the popup is enabled".into());
        }
        if self.popup.min_width > self.popup.max_width {
            issues.push("popup.min_width must not exceed popup.max_width".into());
        }
        if UnicodeWidthStr::width(self.popup.scrollbar_character.as_str()) != 1
            || self.popup.scrollbar_character.chars().any(char::is_control)
        {
            issues.push(
                "popup.scrollbar_character must be exactly one printable terminal cell".into(),
            );
        }
        for (name, style) in self.styles.named_styles() {
            if !valid_zle_highlight(style) {
                issues.push(format!(
                    "styles.{name} must be one non-empty ZLE highlight specification"
                ));
            }
        }
        for (kind, style) in &self.styles.kinds {
            if !COMPLETION_STYLE_KINDS.contains(&kind.as_str()) {
                issues.push(format!(
                    "styles.kinds contains unknown completion kind {kind:?}"
                ));
            }
            if !valid_zle_highlight(style) {
                issues.push(format!(
                    "styles.kinds.{kind} must be one non-empty ZLE highlight specification"
                ));
            }
        }
        if self.matching.max_results == 0 {
            issues.push("matching.max_results must be at least 1".into());
        }
        if !(0.0..=1.0).contains(&self.documentation.width_ratio) {
            issues.push("documentation.width_ratio must be between 0 and 1".into());
        }
        if !(0.0..=1.0).contains(&self.ghost_text.minimum_confidence) {
            issues.push("ghost_text.minimum_confidence must be between 0 and 1".into());
        }
        for (name, soft_timeout_ms, hard_timeout_ms) in self.sources.timeouts() {
            if soft_timeout_ms > hard_timeout_ms {
                issues.push(format!(
                    "sources.{name}.soft_timeout_ms must not exceed hard_timeout_ms"
                ));
            }
        }
        if self.adapters.default_soft_timeout_ms > self.adapters.default_hard_timeout_ms {
            issues.push(
                "adapters.default_soft_timeout_ms must not exceed default_hard_timeout_ms".into(),
            );
        }
        if self.adapters.maximum_concurrency == 0 {
            issues.push("adapters.maximum_concurrency must be at least 1".into());
        }
        if self.cache.memory_mib == 0 {
            issues.push("cache.memory_mib must be at least 1".into());
        }
        if issues.is_empty() {
            Ok(())
        } else {
            Err(ConfigError::Validation(issues))
        }
    }

    /// Generate the JSON Schema used by editors and configuration tooling.
    ///
    /// # Errors
    ///
    /// Returns an error if the generated schema cannot be encoded as JSON.
    pub fn schema_json_pretty() -> Result<String, ConfigError> {
        Ok(serde_json::to_string_pretty(&schema_for!(Self))?)
    }
}

#[derive(Debug, Default, Deserialize)]
struct ProfileSelection {
    profile: Option<String>,
}

fn select_profile(path: &Path, explicit: Option<&str>) -> Result<String, ConfigError> {
    if let Some(profile) = explicit {
        return Ok(profile.to_owned());
    }
    if let Some(profile) = std::env::var_os("ZSH_SENSE_PROFILE") {
        return Ok(profile.to_string_lossy().into_owned());
    }
    if path.is_file() {
        let selection: ProfileSelection = Figment::from(Toml::file(path)).extract()?;
        if let Some(profile) = selection.profile {
            return Ok(profile);
        }
    }
    Ok("default".into())
}

fn validate_profile_name(profile: &str) -> Result<(), ConfigError> {
    let valid = !profile.is_empty()
        && profile.len() <= 64
        && profile
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
        && profile != "."
        && profile != "..";
    if valid {
        Ok(())
    } else {
        Err(ConfigError::InvalidProfile(profile.into()))
    }
}

fn profile_path(config_path: &Path, profile: &str) -> PathBuf {
    config_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join("profiles")
        .join(format!("{profile}.toml"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum ActivationMode {
    Continuous,
    Manual,
    Hybrid,
    Disabled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum TriggerEvent {
    Insert,
    Backspace,
    Delete,
    WordDelete,
    Paste,
    History,
    Cursor,
    Accept,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum CompletionContext {
    Options,
    Paths,
    Subcommands,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct ActivationConfig {
    pub mode: ActivationMode,
    pub debounce_ms: u64,
    pub max_debounce_ms: u64,
    pub min_query_chars: u16,
    pub trigger_on_empty: Vec<CompletionContext>,
    pub events: Vec<TriggerEvent>,
    pub characters: Vec<String>,
    pub immediate_characters: Vec<String>,
    pub after_accept: bool,
}

impl Default for ActivationConfig {
    fn default() -> Self {
        Self {
            mode: ActivationMode::Continuous,
            debounce_ms: 15,
            max_debounce_ms: 75,
            min_query_chars: 1,
            trigger_on_empty: vec![
                CompletionContext::Options,
                CompletionContext::Paths,
                CompletionContext::Subcommands,
            ],
            events: vec![
                TriggerEvent::Insert,
                TriggerEvent::Backspace,
                TriggerEvent::Delete,
                TriggerEvent::WordDelete,
                TriggerEvent::Paste,
                TriggerEvent::History,
                TriggerEvent::Cursor,
                TriggerEvent::Accept,
            ],
            characters: ["/", "-", "=", ":", " "].map(str::to_owned).to_vec(),
            immediate_characters: ["/", "-", "="].map(str::to_owned).to_vec(),
            after_accept: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum KeyAction {
    Trigger,
    Accept,
    Execute,
    Interrupt,
    Next,
    Previous,
    PageDown,
    PageUp,
    Dismiss,
    AcceptNextToken,
    AcceptGhost,
    NextPlaceholder,
    PreviousPlaceholder,
    CancelSnippet,
    PassThrough,
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct KeybindingsConfig {
    pub closed: BTreeMap<String, KeyAction>,
    pub popup: BTreeMap<String, KeyAction>,
    pub snippet: BTreeMap<String, KeyAction>,
}

impl Default for KeybindingsConfig {
    fn default() -> Self {
        Self {
            closed: BTreeMap::from([
                ("tab".into(), KeyAction::Trigger),
                ("ctrl-space".into(), KeyAction::Trigger),
                ("ctrl-c".into(), KeyAction::Interrupt),
                ("enter".into(), KeyAction::Execute),
            ]),
            popup: BTreeMap::from([
                ("tab".into(), KeyAction::Accept),
                ("ctrl-e".into(), KeyAction::Accept),
                ("enter".into(), KeyAction::Execute),
                ("ctrl-c".into(), KeyAction::Interrupt),
                ("ctrl-n".into(), KeyAction::Next),
                ("ctrl-p".into(), KeyAction::Previous),
                ("ctrl-d".into(), KeyAction::PageDown),
                ("ctrl-u".into(), KeyAction::PageUp),
                ("escape".into(), KeyAction::Dismiss),
                ("right".into(), KeyAction::AcceptNextToken),
                ("end".into(), KeyAction::AcceptGhost),
            ]),
            snippet: BTreeMap::from([
                ("tab".into(), KeyAction::NextPlaceholder),
                ("shift-tab".into(), KeyAction::PreviousPlaceholder),
                ("escape".into(), KeyAction::CancelSnippet),
            ]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum CaseMode {
    Smart,
    Sensitive,
    Insensitive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum TypoMode {
    Off,
    Adaptive,
    Fixed,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
#[allow(clippy::struct_excessive_bools)]
pub struct MatchingConfig {
    pub engine: String,
    pub case: CaseMode,
    pub typos: TypoMode,
    pub max_typos: u16,
    pub typo_min_query_chars: u16,
    /// Maximum ranked view items sent to a shell at once. The daemon retains
    /// the bounded source set so later edits can refilter every candidate.
    pub max_results: u32,
    pub preserve_groups: bool,
    pub use_frecency: bool,
    pub use_project_proximity: bool,
    pub explain_scores: bool,
}

impl Default for MatchingConfig {
    fn default() -> Self {
        Self {
            engine: "frizbee".into(),
            case: CaseMode::Smart,
            typos: TypoMode::Adaptive,
            max_typos: 2,
            typo_min_query_chars: 4,
            max_results: 1_000,
            preserve_groups: true,
            use_frecency: true,
            use_project_proximity: true,
            explain_scores: false,
        }
    }
}

macro_rules! string_enum {
    ($name:ident { $($variant:ident),+ $(,)? }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
        #[serde(rename_all = "kebab-case")]
        pub enum $name { $($variant),+ }
    };
}

string_enum!(DecorationMode {
    Full,
    Minimal,
    None
});
string_enum!(BorderStyle {
    Rounded,
    Sharp,
    Ascii,
    None
});
string_enum!(IndicatorMode {
    Icon,
    Text,
    Both,
    None
});
string_enum!(IconTheme {
    NerdFont,
    Unicode,
    Ascii
});
string_enum!(FileIconMode {
    Devicons,
    Generic,
    None
});
string_enum!(DocumentationMode {
    Auto,
    Side,
    Below,
    Manual,
    Off
});
string_enum!(GhostSource {
    Best,
    History,
    Completion
});
string_enum!(PartialAcceptMode {
    Token,
    Word,
    PathSegment,
    Off
});
string_enum!(SnippetSort {
    Top,
    Inline,
    Bottom,
    None
});
string_enum!(ProjectSnippetPolicy {
    TrustedOnly,
    Disabled,
    Always
});
string_enum!(DiagnosticConfidence {
    Authoritative,
    Partial,
    Inferred,
    Advisory
});
string_enum!(ConfirmationMode {
    Off,
    CriticalOnly,
    Configured
});
string_enum!(NativeMode {
    Auto,
    Native,
    Portable
});
string_enum!(ZshCandidateFilter {
    Strict,
    Subsequence
});

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
#[allow(clippy::struct_excessive_bools)]
pub struct PopupConfig {
    pub enabled: bool,
    pub decorations: DecorationMode,
    pub border: BorderStyle,
    pub title: bool,
    pub footer: bool,
    pub scrollbar: bool,
    pub scrollbar_character: String,
    pub group_headings: bool,
    pub descriptions: bool,
    pub max_rows: u16,
    pub max_width: u16,
    pub min_width: u16,
    pub padding: u16,
}

impl Default for PopupConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            decorations: DecorationMode::Full,
            border: BorderStyle::None,
            title: false,
            footer: true,
            scrollbar: true,
            scrollbar_character: "▐".into(),
            group_headings: true,
            descriptions: true,
            max_rows: 10,
            max_width: 140,
            min_width: 24,
            padding: 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct IndicatorConfig {
    pub kinds: IndicatorMode,
    pub icon_theme: IconTheme,
    pub file_icons: FileIconMode,
    pub selected_marker: String,
}

impl Default for IndicatorConfig {
    fn default() -> Self {
        Self {
            kinds: IndicatorMode::Icon,
            icon_theme: IconTheme::NerdFont,
            file_icons: FileIconMode::Devicons,
            selected_marker: String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct DocumentationConfig {
    pub mode: DocumentationMode,
    pub resolve_delay_ms: u64,
    pub side_min_columns: u16,
    pub width_ratio: f32,
    pub max_rows: u16,
    pub render_markdown: bool,
    pub sources: Vec<String>,
}

impl Default for DocumentationConfig {
    fn default() -> Self {
        Self {
            mode: DocumentationMode::Auto,
            resolve_delay_ms: 80,
            side_min_columns: 100,
            width_ratio: 0.45,
            max_rows: 14,
            render_markdown: true,
            sources: ["completion", "schema", "man", "adapter", "help"]
                .map(str::to_owned)
                .to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct GhostTextConfig {
    pub enabled: bool,
    pub source: GhostSource,
    pub minimum_confidence: f32,
    pub at_end_only: bool,
    pub partial_accept: PartialAcceptMode,
}

impl Default for GhostTextConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            source: GhostSource::Best,
            minimum_confidence: 0.82,
            at_end_only: true,
            partial_accept: PartialAcceptMode::Token,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct SourceConfig {
    pub enabled: bool,
    pub priority: i32,
    pub soft_timeout_ms: u64,
    pub hard_timeout_ms: u64,
    pub max_candidates: u32,
}

impl SourceConfig {
    fn cheap(priority: i32) -> Self {
        Self {
            enabled: true,
            priority,
            soft_timeout_ms: 20,
            hard_timeout_ms: 100,
            max_candidates: 100_000,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct ZshSourceConfig {
    pub enabled: bool,
    pub priority: i32,
    pub soft_timeout_ms: u64,
    pub hard_timeout_ms: u64,
    pub max_candidates: u32,
    /// Breadth of the documented matcher injected by the portable backend
    /// before Frizbee performs final filtering and ranking.
    pub candidate_filter: ZshCandidateFilter,
    /// Minimum active-fragment length at which the portable backend broadens
    /// Zsh's candidate universe. Shorter fragments retain native prefix
    /// matching so one keystroke cannot explode into thousands of matches.
    pub fuzzy_min_query_chars: u16,
}

impl Default for ZshSourceConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            priority: 100,
            soft_timeout_ms: 100,
            hard_timeout_ms: 1000,
            max_candidates: 100_000,
            candidate_filter: ZshCandidateFilter::Subsequence,
            fuzzy_min_query_chars: 3,
        }
    }
}

impl Default for SourceConfig {
    fn default() -> Self {
        Self::cheap(50)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct SourcesConfig {
    pub zsh: ZshSourceConfig,
    pub filesystem: FilesystemSourceConfig,
    pub history: SourceConfig,
    pub snippets: SourceConfig,
}

impl SourcesConfig {
    fn timeouts(&self) -> impl Iterator<Item = (&'static str, u64, u64)> {
        [
            ("zsh", self.zsh.soft_timeout_ms, self.zsh.hard_timeout_ms),
            (
                "filesystem",
                self.filesystem.soft_timeout_ms,
                self.filesystem.hard_timeout_ms,
            ),
            (
                "history",
                self.history.soft_timeout_ms,
                self.history.hard_timeout_ms,
            ),
            (
                "snippets",
                self.snippets.soft_timeout_ms,
                self.snippets.hard_timeout_ms,
            ),
        ]
        .into_iter()
    }
}

impl Default for SourcesConfig {
    fn default() -> Self {
        Self {
            zsh: ZshSourceConfig::default(),
            filesystem: FilesystemSourceConfig::default(),
            history: SourceConfig::cheap(40),
            snippets: SourceConfig::cheap(35),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct FilesystemSourceConfig {
    pub enabled: bool,
    pub priority: i32,
    pub soft_timeout_ms: u64,
    pub hard_timeout_ms: u64,
    pub max_candidates: u32,
    pub respect_hidden_prefix: bool,
}

impl Default for FilesystemSourceConfig {
    fn default() -> Self {
        let common = SourceConfig::cheap(80);
        Self {
            enabled: common.enabled,
            priority: common.priority,
            soft_timeout_ms: common.soft_timeout_ms,
            hard_timeout_ms: common.hard_timeout_ms,
            max_candidates: common.max_candidates,
            respect_hidden_prefix: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct AdapterConfig {
    pub enabled: bool,
    pub preview: bool,
    pub soft_timeout_ms: Option<u64>,
    pub hard_timeout_ms: Option<u64>,
}

impl Default for AdapterConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            preview: true,
            soft_timeout_ms: None,
            hard_timeout_ms: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct AdaptersConfig {
    pub enabled: bool,
    pub allow_external: bool,
    pub manifest_paths: Vec<String>,
    pub trusted_adapters: Vec<String>,
    pub default_soft_timeout_ms: u64,
    pub default_hard_timeout_ms: u64,
    pub maximum_concurrency: u16,
    pub git: AdapterConfig,
    pub systemd: AdapterConfig,
    pub external: BTreeMap<String, ExternalAdapterConfig>,
}

impl Default for AdaptersConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            allow_external: true,
            manifest_paths: vec!["~/.config/zsh-sense/adapters/*.json".into()],
            trusted_adapters: Vec::new(),
            default_soft_timeout_ms: 80,
            default_hard_timeout_ms: 500,
            maximum_concurrency: 4,
            git: AdapterConfig::default(),
            systemd: AdapterConfig::default(),
            external: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct ExternalAdapterConfig {
    pub enabled: bool,
    pub manifest: Option<String>,
    pub configuration: BTreeMap<String, serde_json::Value>,
}

impl Default for ExternalAdapterConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            manifest: None,
            configuration: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
#[allow(clippy::struct_excessive_bools)]
pub struct SnippetConfig {
    pub enabled: bool,
    pub format: String,
    pub paths: Vec<String>,
    pub project_snippets: ProjectSnippetPolicy,
    pub linked_placeholders: bool,
    pub choices: bool,
    pub transforms: bool,
    pub sort: SnippetSort,
}

impl Default for SnippetConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            format: "vscode".into(),
            paths: vec!["~/.config/zsh-sense/snippets/*.code-snippets".into()],
            project_snippets: ProjectSnippetPolicy::TrustedOnly,
            linked_placeholders: true,
            choices: true,
            transforms: true,
            sort: SnippetSort::Inline,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct HistoryConfig {
    pub enabled: bool,
    pub incognito: bool,
    pub database: String,
    pub learn_only_successful: bool,
    pub maximum_age_days: u32,
    pub sensitive_patterns: Vec<String>,
}

impl Default for HistoryConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            incognito: false,
            database: "~/.local/state/zsh-sense/history.sqlite3".into(),
            learn_only_successful: true,
            maximum_age_days: 365,
            sensitive_patterns: ["*password*", "*token*", "*secret*", "*Authorization:*"]
                .map(str::to_owned)
                .to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct DiagnosticsConfig {
    pub enabled: bool,
    pub minimum_confidence: DiagnosticConfidence,
    pub show_in_popup: bool,
    pub underline_buffer: bool,
}

impl Default for DiagnosticsConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            minimum_confidence: DiagnosticConfidence::Authoritative,
            show_in_popup: true,
            underline_buffer: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct SafetyConfig {
    pub enabled: bool,
    pub confirmation: ConfirmationMode,
    pub ai_may_block_execution: bool,
}

impl Default for SafetyConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            confirmation: ConfirmationMode::CriticalOnly,
            ai_may_block_execution: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct CacheConfig {
    pub memory_mib: u32,
    pub candidate_ttl_seconds: u64,
    pub documentation_ttl_seconds: u64,
    pub persist_warm_metadata: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            memory_mib: 128,
            candidate_ttl_seconds: 30,
            documentation_ttl_seconds: 3600,
            persist_warm_metadata: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct NativeConfig {
    pub mode: NativeMode,
    pub portable_fallback: bool,
    pub auto_rebuild: bool,
    pub allow_signed_prebuilt: bool,
}

impl Default for NativeConfig {
    fn default() -> Self {
        Self {
            mode: NativeMode::Auto,
            portable_fallback: true,
            auto_rebuild: true,
            allow_signed_prebuilt: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct LoggingConfig {
    pub level: String,
    pub file: String,
    pub include_command_lines: bool,
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            level: "warn".into(),
            file: "~/.local/state/zsh-sense/zsh-sense.log".into(),
            include_command_lines: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct StyleConfig {
    pub menu: String,
    pub border: String,
    pub selected: String,
    pub label: String,
    pub label_match: String,
    pub detail: String,
    pub kind: String,
    pub group: String,
    pub footer: String,
    pub scrollbar_thumb: String,
    pub scrollbar_gutter: String,
    pub diagnostic_error: String,
    pub diagnostic_warning: String,
    pub ghost: String,
    pub kinds: BTreeMap<String, String>,
}

impl Default for StyleConfig {
    fn default() -> Self {
        Self {
            menu: "fg=#bbbbbb,bg=#202020".into(),
            border: "fg=#d4d4d4".into(),
            // This is the user's PmenuSel override in colorscheme.lua. It
            // deliberately specifies only a background so semantic label and
            // kind foregrounds remain visible on the selected row.
            selected: "bg=#343b41".into(),
            label: "fg=#d4d4d4".into(),
            label_match: "fg=#18a2fe,bold".into(),
            detail: "fg=#bbbbbb".into(),
            kind: "fg=#bbbbbb".into(),
            group: "fg=#4ec9b0".into(),
            footer: "fg=#bbbbbb".into(),
            scrollbar_thumb: "fg=#bbbbbb".into(),
            scrollbar_gutter: "fg=#343b41".into(),
            diagnostic_error: "fg=#f14c4c,underline".into(),
            diagnostic_warning: "fg=#cca700,underline".into(),
            ghost: "fg=#707070".into(),
            kinds: blink_kind_styles(),
        }
    }
}

impl StyleConfig {
    fn named_styles(&self) -> [(&'static str, &str); 14] {
        [
            ("menu", &self.menu),
            ("border", &self.border),
            ("selected", &self.selected),
            ("label", &self.label),
            ("label_match", &self.label_match),
            ("detail", &self.detail),
            ("kind", &self.kind),
            ("group", &self.group),
            ("footer", &self.footer),
            ("scrollbar_thumb", &self.scrollbar_thumb),
            ("scrollbar_gutter", &self.scrollbar_gutter),
            ("diagnostic_error", &self.diagnostic_error),
            ("diagnostic_warning", &self.diagnostic_warning),
            ("ghost", &self.ghost),
        ]
    }
}

const COMPLETION_STYLE_KINDS: &[&str] = &[
    "text",
    "command",
    "alias",
    "builtin",
    "function",
    "subcommand",
    "option",
    "option-value",
    "variable",
    "file",
    "directory",
    "symlink",
    "user",
    "host",
    "process",
    "job",
    "git-branch",
    "git-tag",
    "git-commit",
    "service",
    "container",
    "image",
    "package",
    "history",
    "snippet",
    "action",
];

fn blink_kind_styles() -> BTreeMap<String, String> {
    [
        ("text", "fg=#bbbbbb"),
        ("command", "fg=#c586c0"),
        ("alias", "fg=#c586c0"),
        ("builtin", "fg=#c586c0"),
        ("function", "fg=#c586c0"),
        ("subcommand", "fg=#c586c0"),
        ("option", "fg=#ffd602"),
        ("option-value", "fg=#9cdcfe"),
        ("variable", "fg=#9cdcfe"),
        ("file", "fg=#d4d4d4"),
        ("directory", "fg=#569cd6"),
        ("symlink", "fg=#d4d4d4"),
        ("snippet", "fg=#ffd602"),
        ("action", "fg=#ffd602"),
    ]
    .into_iter()
    .map(|(kind, style)| (kind.into(), style.into()))
    .collect()
}

fn valid_zle_highlight(style: &str) -> bool {
    !style.is_empty()
        && !style
            .chars()
            .any(|character| character.is_whitespace() || character == '\0')
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RuleMatch {
    pub commands: Vec<String>,
    pub cwd: Vec<String>,
    pub keymaps: Vec<String>,
    pub environment: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RuleActivation {
    pub mode: Option<ActivationMode>,
    pub debounce_ms: Option<u64>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RuleHistory {
    pub enabled: Option<bool>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RulePopup {
    pub decorations: Option<DecorationMode>,
    pub max_rows: Option<u16>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RuleIndicators {
    pub kinds: Option<IndicatorMode>,
    pub file_icons: Option<FileIconMode>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct RuleConfig {
    pub name: String,
    #[serde(rename = "match")]
    pub selector: RuleMatch,
    pub activation: RuleActivation,
    pub history: RuleHistory,
    pub popup: RulePopup,
    pub indicators: RuleIndicators,
}
