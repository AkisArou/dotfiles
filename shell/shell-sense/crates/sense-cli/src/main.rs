use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;
use std::time::Duration;

use anyhow::{Context, Result};
use clap::{ArgGroup, Args, Parser, Subcommand};
use sense_config::{
    BorderStyle, Config, ConfigPaths, DocumentationMode, IndicatorMode, KeyAction,
    ZshCandidateFilter,
};
use sense_daemon::{Server, ServerConfig};
use sense_model::{NativeShell, RawBytes};
use sense_present::DocumentationPlacementPreference;
use sense_protocol::ShellIdentity;
use sense_shell_worker::{
    BridgeConfig, CaptureLimits, DocumentationActivation, DocumentationLayoutPolicy,
    DocumentationPolicy, GhostTextPolicy, MenuLayoutPolicy, ShellWireMessage,
};
use serde::Serialize;
use tracing_subscriber::EnvFilter;
use unicode_width::UnicodeWidthStr;

mod daemon_lifecycle;
mod install;

#[derive(Debug, Parser)]
#[command(
    name = "shell-sense",
    version,
    about = "Continuous, native completion IntelliSense for interactive shells"
)]
struct Cli {
    #[command(subcommand)]
    command: CommandKind,
}

#[derive(Debug, Subcommand)]
enum CommandKind {
    /// Run the persistent completion daemon.
    Daemon(DaemonArgs),
    /// Run one persistent bridge for an interactive shell process.
    Worker(WorkerArgs),
    /// Present a live terminal shell session through Blink.cmp.
    Blink(BlinkArgs),
    /// Inspect and validate configuration.
    Config(ConfigArgs),
    /// Check the local environment and installation prerequisites.
    Doctor(DoctorArgs),
    /// Print shell initialization code.
    Init(InitArgs),
    /// Install the executable and exact runtime asset tree for the current user.
    Install(InstallArgs),
}

#[derive(Debug, Args)]
struct DaemonArgs {
    /// Override the Unix socket path.
    #[arg(long)]
    socket: Option<PathBuf>,
    /// Override the configuration file.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Select a named profile from the adjacent profiles directory.
    #[arg(long)]
    profile: Option<String>,
}

#[derive(Debug, Args)]
struct BlinkArgs {
    /// Owning terminal shell process to attach to.
    #[arg(long)]
    shell_process_id: u32,
    /// Override the Unix socket path.
    #[arg(long)]
    socket: Option<PathBuf>,
    /// Maximum time to wait for the terminal shell session to register.
    #[arg(long, default_value_t = 3000)]
    attach_timeout_ms: u64,
}

#[derive(Debug, Args)]
#[command(group(
    ArgGroup::new("shell_output")
        .args(["shell_output_fifo", "shell_output_mailbox"])
        .multiple(false)
))]
struct WorkerArgs {
    /// Override the Unix socket path.
    #[arg(long)]
    socket: Option<PathBuf>,
    /// Override the configuration file.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Select a named profile from the adjacent profiles directory.
    #[arg(long)]
    profile: Option<String>,
    /// FIFO carrying framed messages from the owning shell to this worker.
    #[arg(long, requires = "shell_output")]
    shell_input_fifo: Option<PathBuf>,
    /// FIFO carrying messages from this worker to Zsh.
    #[arg(long, requires = "shell_input_fifo")]
    shell_output_fifo: Option<PathBuf>,
    /// Acknowledged mailbox carrying worker responses to Fish/Bash.
    #[arg(
        long,
        requires = "shell_input_fifo",
        conflicts_with = "shell_output_fifo"
    )]
    shell_output_mailbox: Option<PathBuf>,
    /// Owning interactive shell process.
    #[arg(long)]
    shell_process_id: u32,
    /// Do not start the daemon when it is not already reachable.
    #[arg(long)]
    no_daemon_autostart: bool,
    /// Native completion authority for this interactive session.
    #[arg(long)]
    shell: Shell,
    /// Executable path reported by the owning interactive shell.
    #[arg(long)]
    shell_executable: String,
    /// Version reported by the owning interactive shell.
    #[arg(long)]
    shell_version: String,
    /// Optional implementation patch level.
    #[arg(long)]
    shell_patchlevel: Option<String>,
}

#[derive(Debug, Args)]
struct ConfigArgs {
    #[command(subcommand)]
    command: ConfigCommand,
}

#[derive(Debug, Subcommand)]
enum ConfigCommand {
    /// Validate the merged configuration.
    Check(ConfigPathArgs),
    /// Print the fully merged and validated configuration as JSON.
    Effective(ConfigPathArgs),
    /// Print the generated JSON Schema.
    Schema,
    /// Print discovered configuration, state, cache, and runtime paths.
    Paths,
}

#[derive(Debug, Args)]
struct ConfigPathArgs {
    /// Override the configuration file.
    #[arg(long)]
    path: Option<PathBuf>,
    /// Select a named profile from the adjacent profiles directory.
    #[arg(long)]
    profile: Option<String>,
}

#[derive(Debug, Args)]
struct DoctorArgs {
    /// Override the configuration file.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Select a named profile from the adjacent profiles directory.
    #[arg(long)]
    profile: Option<String>,
    /// Shell integration to inspect; defaults to the current SHELL.
    #[arg(long, value_enum)]
    shell: Option<Shell>,
}

#[derive(Debug, Clone, Args)]
struct InitArgs {
    #[arg(value_enum, default_value_t = Shell::Zsh)]
    shell: Shell,
    /// Override the installed runtime asset directory.
    #[arg(long)]
    data_dir: Option<PathBuf>,
}

#[derive(Debug, Clone, Args)]
struct InstallArgs {
    /// Override the directory receiving the shell-sense executable.
    #[arg(long)]
    bin_dir: Option<PathBuf>,
    /// Override the directory receiving shell and Blink runtime assets.
    #[arg(long)]
    data_dir: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum Shell {
    Zsh,
    Fish,
    Bash,
}

impl From<Shell> for NativeShell {
    fn from(value: Shell) -> Self {
        match value {
            Shell::Zsh => Self::Zsh,
            Shell::Fish => Self::Fish,
            Shell::Bash => Self::Bash,
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        CommandKind::Daemon(arguments) => run_daemon(arguments).await,
        CommandKind::Worker(arguments) => run_worker(arguments).await,
        CommandKind::Blink(arguments) => run_blink(arguments).await,
        CommandKind::Config(arguments) => run_config(arguments),
        CommandKind::Doctor(arguments) => run_doctor(&arguments),
        CommandKind::Init(arguments) => {
            run_init(&arguments)?;
            Ok(())
        }
        CommandKind::Install(arguments) => {
            run_install(&arguments)?;
            Ok(())
        }
    }
}

async fn run_blink(arguments: BlinkArgs) -> Result<()> {
    let paths = ConfigPaths::discover()?;
    let socket = arguments.socket.unwrap_or_else(|| paths.socket_path());
    let mut config = sense_blink::BridgeConfig::new(socket, arguments.shell_process_id);
    config.attach_timeout = Duration::from_millis(arguments.attach_timeout_ms);
    sense_blink::run(config).await?;
    Ok(())
}

async fn run_worker(arguments: WorkerArgs) -> Result<()> {
    let config =
        Config::load_with_profile(arguments.config.as_deref(), arguments.profile.as_deref())?;
    initialize_logging(&config.logging.level)?;
    let paths = ConfigPaths::discover()?;
    let socket = arguments.socket.unwrap_or_else(|| paths.socket_path());
    let shell = ShellIdentity {
        shell: arguments.shell.into(),
        executable: arguments.shell_executable,
        version: arguments.shell_version,
        patchlevel: arguments.shell_patchlevel,
    };
    if !arguments.no_daemon_autostart {
        daemon_lifecycle::ensure_daemon(
            &socket,
            arguments.config.as_deref(),
            arguments.profile.as_deref(),
            &shell,
        )
        .await?;
    }
    let mut bridge = BridgeConfig::new(socket, shell);
    bridge.shell_process_id = arguments.shell_process_id;
    bridge.capture_limits = CaptureLimits {
        max_candidates: match bridge.shell.shell {
            NativeShell::Zsh => config.sources.zsh.max_candidates,
            NativeShell::Fish => config.sources.fish.max_candidates,
            NativeShell::Bash => config.sources.bash.max_candidates,
        } as usize,
        ..CaptureLimits::default()
    };
    // Bash programmable completion executes synchronously inside Readline.
    // Holding the editing widget for an additional debounce would only add
    // latency; superseding requests still provide cancellation semantics.
    bridge.debounce = if bridge.shell.shell == NativeShell::Bash {
        Duration::ZERO
    } else {
        Duration::from_millis(config.activation.debounce_ms)
    };
    // Keep one look-ahead page in the per-shell worker. Normal next/previous
    // and a full page-down remain synchronous in ZLE, while hundreds of
    // off-screen candidates never cross the latency-sensitive shell boundary.
    bridge.viewport_rows = (config.popup.max_rows as usize).saturating_mul(2);
    bridge.ghost_text = GhostTextPolicy {
        enabled: config.ghost_text.enabled,
        minimum_confidence: config.ghost_text.minimum_confidence,
    };
    bridge.documentation = DocumentationPolicy {
        activation: if matches!(
            config.documentation.mode,
            DocumentationMode::Off | DocumentationMode::Manual
        ) {
            DocumentationActivation::Disabled
        } else {
            DocumentationActivation::Automatic
        },
        resolve_delay: Duration::from_millis(config.documentation.resolve_delay_ms),
        layout: DocumentationLayoutPolicy {
            placement: match config.documentation.mode {
                DocumentationMode::Side => DocumentationPlacementPreference::Side,
                DocumentationMode::Below => DocumentationPlacementPreference::Below,
                DocumentationMode::Auto | DocumentationMode::Manual | DocumentationMode::Off => {
                    DocumentationPlacementPreference::Auto
                }
            },
            side_min_columns: config.documentation.side_min_columns,
            width_ratio: config.documentation.width_ratio,
            max_rows: config.documentation.max_rows,
            render_markdown: config.documentation.render_markdown,
            padding: config.popup.padding,
            bordered: config.popup.border != BorderStyle::None,
        },
        menu: MenuLayoutPolicy {
            menu_min_width: config.popup.min_width,
            menu_max_width: config.popup.max_width,
            menu_max_rows: config.popup.max_rows,
            menu_chrome_cells: menu_chrome_cells(&config),
            scrollbar: config.popup.scrollbar,
            descriptions: config.popup.descriptions,
        },
    };
    bridge.startup_messages = shell_startup_messages(&config, bridge.shell.shell)?;
    match (
        arguments.shell_input_fifo,
        arguments.shell_output_fifo,
        arguments.shell_output_mailbox,
    ) {
        (Some(input), Some(output), None) => {
            sense_shell_worker::run_fifo_bridge(bridge, input, output).await?;
        }
        (Some(input), None, Some(output)) => {
            sense_shell_worker::run_signal_bridge(
                bridge,
                input,
                output,
                arguments.shell_process_id,
            )
            .await?;
        }
        (None, None, None) => sense_shell_worker::run_stdio_bridge(bridge).await?,
        _ => unreachable!("clap validates complete, exclusive transports"),
    }
    Ok(())
}

fn menu_chrome_cells(config: &Config) -> u16 {
    let marker = u16::try_from(UnicodeWidthStr::width(
        config.indicators.selected_marker.as_str(),
    ))
    .unwrap_or(u16::MAX);
    let marker = marker.saturating_add(u16::from(marker > 0));
    let indicator = match config.indicators.kinds {
        IndicatorMode::Icon => 2,
        IndicatorMode::Text => 4,
        IndicatorMode::Both => 17,
        IndicatorMode::None => 0,
    };
    config
        .popup
        .padding
        .saturating_mul(2)
        .saturating_add(u16::from(config.popup.border != BorderStyle::None).saturating_mul(2))
        .saturating_add(marker)
        .saturating_add(indicator)
}

fn shell_startup_messages(config: &Config, shell: NativeShell) -> Result<Vec<ShellWireMessage>> {
    let popup = &config.popup;
    let mut messages = vec![
        ShellWireMessage::new("config", shell_config_fields(config, shell)?),
        ShellWireMessage::new(
            "popup-option",
            vec![
                "scrollbar-character".into(),
                popup.scrollbar_character.as_str().into(),
            ],
        ),
        ghost_startup_message(config)?,
    ];
    for (name, value) in [
        ("menu", &config.styles.menu),
        ("border", &config.styles.border),
        ("selected", &config.styles.selected),
        ("label", &config.styles.label),
        ("label-match", &config.styles.label_match),
        ("detail", &config.styles.detail),
        ("kind", &config.styles.kind),
        ("group", &config.styles.group),
        ("footer", &config.styles.footer),
        ("scrollbar-thumb", &config.styles.scrollbar_thumb),
        ("scrollbar-gutter", &config.styles.scrollbar_gutter),
        ("ghost", &config.styles.ghost),
        ("documentation", &config.styles.documentation),
        ("documentation-border", &config.styles.documentation_border),
        (
            "documentation-heading",
            &config.styles.documentation_heading,
        ),
        ("documentation-code", &config.styles.documentation_code),
        ("documentation-quote", &config.styles.documentation_quote),
    ] {
        messages.push(ShellWireMessage::new(
            "style",
            vec![name.into(), value.as_str().into()],
        ));
    }
    for (kind, value) in &config.styles.kinds {
        messages.push(ShellWireMessage::new(
            "kind-style",
            vec![kind.as_str().into(), value.as_str().into()],
        ));
    }
    for (state, bindings) in [
        ("closed", &config.keybindings.closed),
        ("popup", &config.keybindings.popup),
    ] {
        for (key, action) in bindings {
            if !shell_supports_key_action(shell, *action) {
                continue;
            }
            messages.push(ShellWireMessage::new(
                "keybinding",
                vec![state.into(), key.as_str().into(), enum_name(action)?.into()],
            ));
        }
    }
    messages.push(ShellWireMessage::new("config-end", Vec::new()));
    Ok(messages)
}

fn shell_supports_key_action(shell: NativeShell, action: KeyAction) -> bool {
    match action {
        KeyAction::AcceptNextToken | KeyAction::AcceptGhost => shell == NativeShell::Zsh,
        _ => true,
    }
}

fn shell_config_fields(config: &Config, shell: NativeShell) -> Result<Vec<RawBytes>> {
    let activation = &config.activation;
    let popup = &config.popup;
    let indicators = &config.indicators;
    let mut fields = vec![
        enum_name(activation.mode)?.into(),
        activation.debounce_ms.to_string().into(),
        bool_bytes(activation.after_accept),
        bool_bytes(popup.enabled),
        popup.max_rows.to_string().into(),
        popup.max_width.to_string().into(),
        popup.min_width.to_string().into(),
        popup.padding.to_string().into(),
        enum_name(popup.decorations)?.into(),
        enum_name(popup.border)?.into(),
        bool_bytes(popup.title),
        bool_bytes(popup.footer),
        bool_bytes(popup.scrollbar),
        bool_bytes(popup.group_headings),
        bool_bytes(popup.descriptions),
        config.styles.detail.as_str().into(),
        enum_name(indicators.kinds)?.into(),
        indicators.selected_marker.as_str().into(),
        if shell == NativeShell::Zsh {
            zsh_candidate_matcher(config.sources.zsh.candidate_filter).into()
        } else {
            RawBytes::default()
        },
        match shell {
            NativeShell::Zsh => config.sources.zsh.fuzzy_min_query_chars,
            NativeShell::Fish => config.sources.fish.fuzzy_min_query_chars,
            NativeShell::Bash => config.sources.bash.fuzzy_min_query_chars,
        }
        .to_string()
        .into(),
        activation.characters.len().to_string().into(),
    ];
    fields.extend(
        activation
            .characters
            .iter()
            .map(|value| RawBytes::from(value.as_str())),
    );
    fields.push(activation.immediate_characters.len().to_string().into());
    fields.extend(
        activation
            .immediate_characters
            .iter()
            .map(|value| RawBytes::from(value.as_str())),
    );
    fields.push(activation.events.len().to_string().into());
    for event in &activation.events {
        fields.push(enum_name(event)?.into());
    }
    Ok(fields)
}

fn ghost_startup_message(config: &Config) -> Result<ShellWireMessage> {
    Ok(ShellWireMessage::new(
        "ghost-config",
        vec![
            bool_bytes(config.ghost_text.enabled),
            enum_name(config.ghost_text.partial_accept)?.into(),
        ],
    ))
}

fn enum_name(value: impl Serialize) -> Result<String> {
    serde_json::to_value(value)?
        .as_str()
        .map(ToOwned::to_owned)
        .context("configuration enum did not serialize as a string")
}

fn bool_bytes(value: bool) -> RawBytes {
    RawBytes::from(if value { "1" } else { "0" })
}

fn zsh_candidate_matcher(filter: ZshCandidateFilter) -> &'static str {
    match filter {
        ZshCandidateFilter::Strict => "",
        ZshCandidateFilter::Subsequence => "r:|?=**",
    }
}

async fn run_daemon(arguments: DaemonArgs) -> Result<()> {
    let config =
        Config::load_with_profile(arguments.config.as_deref(), arguments.profile.as_deref())?;
    initialize_logging(&config.logging.level)?;
    let paths = ConfigPaths::discover()?;
    let socket = arguments.socket.unwrap_or_else(|| paths.socket_path());
    let server = Server::bind(ServerConfig::new(&socket).with_product_config(&config))?;
    tracing::info!(path = %socket.display(), "shell-sense daemon listening");
    server
        .run_until(async {
            if let Err(error) = tokio::signal::ctrl_c().await {
                tracing::error!(%error, "could not install Ctrl-C handler");
            }
        })
        .await?;
    Ok(())
}

fn initialize_logging(default_level: &str) -> Result<()> {
    let filter = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new(default_level))
        .context("invalid logging filter")?;
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(false)
        .try_init()
        .map_err(|error| anyhow::anyhow!("logging could not be initialized: {error}"))?;
    Ok(())
}

fn run_config(arguments: ConfigArgs) -> Result<()> {
    match arguments.command {
        ConfigCommand::Check(arguments) => {
            let path = selected_config_path(arguments.path.as_deref())?;
            Config::load_with_profile(arguments.path.as_deref(), arguments.profile.as_deref())?;
            println!("configuration is valid: {}", path.display());
        }
        ConfigCommand::Effective(arguments) => {
            let config =
                Config::load_with_profile(arguments.path.as_deref(), arguments.profile.as_deref())?;
            println!("{}", serde_json::to_string_pretty(&config)?);
        }
        ConfigCommand::Schema => println!("{}", Config::schema_json_pretty()?),
        ConfigCommand::Paths => print_paths(&ConfigPaths::discover()?),
    }
    Ok(())
}

fn run_doctor(arguments: &DoctorArgs) -> Result<()> {
    let paths = ConfigPaths::discover()?;
    let config_path = arguments
        .config
        .clone()
        .unwrap_or_else(|| paths.config_file.clone());
    Config::load_with_profile(arguments.config.as_deref(), arguments.profile.as_deref())?;

    println!("configuration  ok  {}", config_path.display());
    println!("daemon socket  {}", paths.socket_path().display());
    match doctor_shell(arguments.shell)? {
        Shell::Zsh => doctor_zsh()?,
        Shell::Fish => doctor_command("fish", &["--version"])?,
        Shell::Bash => doctor_command("bash", &["--version"])?,
    }
    Ok(())
}

fn doctor_shell(explicit: Option<Shell>) -> Result<Shell> {
    if let Some(shell) = explicit {
        return Ok(shell);
    }
    let executable = std::env::var_os("SHELL")
        .and_then(|path| {
            PathBuf::from(path)
                .file_name()
                .map(std::borrow::ToOwned::to_owned)
        })
        .and_then(|name| name.to_str().map(str::to_owned))
        .unwrap_or_default();
    match executable.as_str() {
        "zsh" => Ok(Shell::Zsh),
        "fish" => Ok(Shell::Fish),
        "bash" => Ok(Shell::Bash),
        _ => anyhow::bail!("cannot infer a supported shell; pass --shell zsh, fish, or bash"),
    }
}

fn doctor_zsh() -> Result<()> {
    let output = ProcessCommand::new("zsh")
        .args([
            "-fc",
            "zmodload zsh/complete && print -r -- $ZSH_VERSION && print -r -- $ZSH_PATCHLEVEL",
        ])
        .output()
        .context("could not execute zsh")?;
    if !output.status.success() {
        anyhow::bail!(
            "zsh completion module probe exited with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut lines = stdout.lines();
    let version = lines.next().unwrap_or("unknown");
    let patchlevel = lines.next().unwrap_or("unknown");
    println!("shell           ok  zsh");
    println!("version         ok  {version}");
    println!("patchlevel          {patchlevel}");
    println!("completion module   available");
    println!("completion provider  shell");
    Ok(())
}

fn doctor_command(command: &str, arguments: &[&str]) -> Result<()> {
    let output = ProcessCommand::new(command)
        .args(arguments)
        .output()
        .with_context(|| format!("could not execute {command}"))?;
    if !output.status.success() {
        anyhow::bail!("{command} version probe exited with {}", output.status);
    }
    let version = String::from_utf8_lossy(&output.stdout);
    let version = version.lines().next().unwrap_or("unknown");
    println!("shell           ok  {command}");
    println!("version         ok  {version}");
    println!("completion provider  shell");
    Ok(())
}

fn run_init(arguments: &InitArgs) -> Result<()> {
    let data_directory = arguments
        .data_dir
        .clone()
        .unwrap_or(ConfigPaths::discover()?.data_dir);
    let entry_point = match arguments.shell {
        Shell::Zsh => data_directory.join("shell/zsh/shell-sense.plugin.zsh"),
        Shell::Fish => data_directory.join("shell/fish/shell-sense.fish"),
        Shell::Bash => data_directory.join("shell/bash/shell-sense.bash"),
    };
    let entry_point = entry_point
        .to_str()
        .context("shell initialization path is not valid UTF-8")?;
    let entry_point = shell_quote(entry_point);
    match arguments.shell {
        Shell::Zsh => {
            println!("# Add this after compinit in .zshrc:");
            println!("source {entry_point}");
        }
        Shell::Fish => {
            println!("# Add this to config.fish:");
            println!("source {entry_point}");
        }
        Shell::Bash => {
            println!("# Add this to .bashrc:");
            println!("source {entry_point}");
        }
    }
    Ok(())
}

fn run_install(arguments: &InstallArgs) -> Result<()> {
    let executable = std::env::current_exe().context("could not locate the running executable")?;
    let data = arguments
        .data_dir
        .clone()
        .unwrap_or(ConfigPaths::discover()?.data_dir);
    let bin_directory = arguments
        .bin_dir
        .clone()
        .unwrap_or(etcetera::home_dir()?.join(".local/bin"));
    let paths = install::InstallPaths {
        executable: bin_directory.join("shell-sense"),
        data,
    };
    install::install(&executable, &paths)?;
    println!("executable  {}", paths.executable.display());
    println!("assets      {}", paths.data.display());
    println!("run `shell-sense init <zsh|fish|bash>` for the source line");
    Ok(())
}

fn shell_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\\''"))
}

fn selected_config_path(explicit: Option<&Path>) -> Result<PathBuf> {
    Ok(match explicit {
        Some(path) => path.to_path_buf(),
        None => ConfigPaths::discover()?.config_file,
    })
}

fn print_paths(paths: &ConfigPaths) {
    println!("config  {}", paths.config_file.display());
    println!("profiles {}", paths.profiles_dir.display());
    println!("data    {}", paths.data_dir.display());
    println!("cache   {}", paths.cache_dir.display());
    println!(
        "state   {}",
        paths
            .state_dir
            .as_deref()
            .map_or_else(|| "unavailable".into(), |path| path.display().to_string())
    );
    println!(
        "runtime {}",
        paths
            .runtime_dir
            .as_deref()
            .map_or_else(|| "unavailable".into(), |path| path.display().to_string())
    );
    println!("socket  {}", paths.socket_path().display());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ghost_acceptance_bindings_are_only_advertised_to_zsh() {
        for shell in [NativeShell::Fish, NativeShell::Bash] {
            assert!(!shell_supports_key_action(
                shell,
                KeyAction::AcceptNextToken
            ));
            assert!(!shell_supports_key_action(shell, KeyAction::AcceptGhost));
            assert!(shell_supports_key_action(shell, KeyAction::Accept));
        }
        assert!(shell_supports_key_action(
            NativeShell::Zsh,
            KeyAction::AcceptNextToken
        ));
        assert!(shell_supports_key_action(
            NativeShell::Zsh,
            KeyAction::AcceptGhost
        ));
    }

    #[test]
    fn initialization_paths_are_shell_quoted() {
        assert_eq!(
            shell_quote("/tmp/shell sense's"),
            "'/tmp/shell sense'\\''s'"
        );
    }
}
