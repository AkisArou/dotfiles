use std::path::{Path, PathBuf};
use std::process::{Command as ProcessCommand, Stdio};
use std::time::Duration;

use anyhow::{Context, Result};
use clap::{Args, Parser, Subcommand};
use sense_config::{Config, ConfigPaths, ZshCandidateFilter};
use sense_daemon::{Server, ServerConfig};
use sense_model::RawBytes;
use sense_protocol::ZshIdentity;
use sense_zsh_worker::{BridgeConfig, CaptureLimits, ShellWireMessage};
use serde::Serialize;
use tokio::net::UnixStream;
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(
    name = "zsh-sense",
    version,
    about = "Continuous, context-aware IntelliSense for Zsh"
)]
struct Cli {
    #[command(subcommand)]
    command: CommandKind,
}

#[derive(Debug, Subcommand)]
enum CommandKind {
    /// Run the persistent completion daemon.
    Daemon(DaemonArgs),
    /// Run one persistent bridge for an interactive Zsh process.
    Worker(WorkerArgs),
    /// Inspect and validate configuration.
    Config(ConfigArgs),
    /// Check the local environment and installation prerequisites.
    Doctor(DoctorArgs),
    /// Print shell initialization code.
    Init(InitArgs),
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
    /// FIFO carrying messages from Zsh to this worker.
    #[arg(long, requires = "shell_output_fifo")]
    shell_input_fifo: Option<PathBuf>,
    /// FIFO carrying messages from this worker to Zsh.
    #[arg(long, requires = "shell_input_fifo")]
    shell_output_fifo: Option<PathBuf>,
    /// Do not start the daemon when it is not already reachable.
    #[arg(long)]
    no_daemon_autostart: bool,
    /// Executable path reported by the owning interactive Zsh.
    #[arg(long, requires = "zsh_version")]
    zsh_executable: Option<String>,
    /// Version reported by the owning interactive Zsh.
    #[arg(long, requires = "zsh_executable")]
    zsh_version: Option<String>,
    /// Patchlevel reported by the owning interactive Zsh.
    #[arg(long)]
    zsh_patchlevel: Option<String>,
    /// Verified native adapter ABI key, when native capture is active.
    #[arg(long)]
    native_abi_key: Option<String>,
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
}

#[derive(Debug, Clone, Copy, Args)]
struct InitArgs {
    #[arg(value_enum, default_value_t = Shell::Zsh)]
    shell: Shell,
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum Shell {
    Zsh,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        CommandKind::Daemon(arguments) => run_daemon(arguments).await,
        CommandKind::Worker(arguments) => run_worker(arguments).await,
        CommandKind::Config(arguments) => run_config(arguments),
        CommandKind::Doctor(arguments) => run_doctor(&arguments),
        CommandKind::Init(arguments) => {
            run_init(arguments);
            Ok(())
        }
    }
}

async fn run_worker(arguments: WorkerArgs) -> Result<()> {
    let config =
        Config::load_with_profile(arguments.config.as_deref(), arguments.profile.as_deref())?;
    initialize_logging(&config.logging.level)?;
    let paths = ConfigPaths::discover()?;
    let socket = arguments.socket.unwrap_or_else(|| paths.socket_path());
    if !arguments.no_daemon_autostart {
        ensure_daemon(
            &socket,
            arguments.config.as_deref(),
            arguments.profile.as_deref(),
        )
        .await?;
    }
    let zsh = arguments
        .zsh_executable
        .zip(arguments.zsh_version)
        .map(|(executable, version)| ZshIdentity {
            executable,
            version,
            patchlevel: arguments.zsh_patchlevel,
            native_abi_key: arguments.native_abi_key,
        });
    let mut bridge = BridgeConfig::new(socket);
    bridge.zsh = zsh;
    bridge.capture_limits = CaptureLimits {
        max_candidates: config.sources.zsh.max_candidates as usize,
        ..CaptureLimits::default()
    };
    bridge.debounce = Duration::from_millis(config.activation.debounce_ms);
    bridge.startup_messages = shell_startup_messages(&config)?;
    match (arguments.shell_input_fifo, arguments.shell_output_fifo) {
        (Some(input), Some(output)) => {
            sense_zsh_worker::run_fifo_bridge(bridge, input, output).await?;
        }
        (None, None) => sense_zsh_worker::run_stdio_bridge(bridge).await?,
        _ => unreachable!("clap requires both FIFO arguments"),
    }
    Ok(())
}

async fn ensure_daemon(socket: &Path, config: Option<&Path>, profile: Option<&str>) -> Result<()> {
    if UnixStream::connect(socket).await.is_ok() {
        return Ok(());
    }

    let executable =
        std::env::current_exe().context("could not locate the zsh-sense executable")?;
    let mut command = ProcessCommand::new(executable);
    command
        .arg("daemon")
        .arg("--socket")
        .arg(socket)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    if let Some(config) = config {
        command.arg("--config").arg(config);
    }
    if let Some(profile) = profile {
        command.arg("--profile").arg(profile);
    }
    let mut child = command
        .spawn()
        .context("could not start zsh-sense daemon")?;
    for _ in 0..200 {
        if UnixStream::connect(socket).await.is_ok() {
            return Ok(());
        }
        if let Some(status) = child
            .try_wait()
            .context("could not inspect zsh-sense daemon")?
        {
            anyhow::bail!("zsh-sense daemon exited during startup with {status}");
        }
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    anyhow::bail!(
        "zsh-sense daemon did not create {} within 2 seconds",
        socket.display()
    )
}

fn shell_startup_messages(config: &Config) -> Result<Vec<ShellWireMessage>> {
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
        enum_name(indicators.kinds)?.into(),
        indicators.selected_marker.as_str().into(),
        zsh_candidate_matcher(config.sources.zsh.candidate_filter).into(),
        config.sources.zsh.fuzzy_min_query_chars.to_string().into(),
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

    let mut messages = vec![ShellWireMessage::new("config", fields)];
    for (state, bindings) in [
        ("closed", &config.keybindings.closed),
        ("popup", &config.keybindings.popup),
        ("snippet", &config.keybindings.snippet),
    ] {
        for (key, action) in bindings {
            messages.push(ShellWireMessage::new(
                "keybinding",
                vec![state.into(), key.as_str().into(), enum_name(action)?.into()],
            ));
        }
    }
    messages.push(ShellWireMessage::new("config-end", Vec::new()));
    Ok(messages)
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
    tracing::info!(path = %socket.display(), "zsh-sense daemon listening");
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

    let identity = sense_zsh_abi::probe("zsh")?;
    println!("zsh executable ok  {}", identity.executable.display());
    println!("zsh version    ok  {}", identity.version);
    println!("zsh patchlevel     {}", identity.patchlevel);
    println!("module suffix      .{}", identity.module_suffix);
    println!(
        "dynamic modules   {}",
        if identity.dynamic_modules {
            "ok"
        } else {
            "unavailable"
        }
    );
    println!("native ABI key     {}", identity.abi_key());
    println!("native adapter     not installed (portable mode remains available)");
    Ok(())
}

fn run_init(arguments: InitArgs) {
    match arguments.shell {
        Shell::Zsh => {
            println!("# Add this after compinit in .zshrc:");
            println!("source \"$HOME/.local/share/zsh-sense/shell/zsh-sense.plugin.zsh\"");
        }
    }
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
