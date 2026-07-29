# Shell Sense

Shell Sense is a continuous, editor-style completion client for Zsh, Fish,
and Bash. Each interactive shell remains the sole authority for candidate
generation and acceptance; Shell Sense adds fuzzy ranking, a shared popup,
completion-derived ghost text, and a documentation/enrichment protocol.

The design and delivery gates are documented in [PLAN.md](PLAN.md).

## Current state

- Zsh works end to end through `_main_complete` and ZLE.
- Fish works end to end through live `complete -C` and `commandline`.
- Bash has a native programmable-completion provider and an interactive
  Readline client. Bash continuous completion reconstructs the public
  `COMP_*` context because Bash has no arbitrary-line completion API.
- Frizbee performs fuzzy scoring and returns match spans used by every client.
- The daemon rejects candidates that do not come from the active shell's
  completion worker.
- Context adapters may enrich or document an existing native item, but cannot
  add candidates.
- Native descriptions remain concise menu details; they are not repeated in
  the documentation pane. Independently resolved documentation renders in a
  responsive side/below pane in all three clients. Markdown parsing, Unicode
  wrapping, a scrollable viewport, delayed resolve, and stale-generation
  rejection are shared Rust behavior.
- Built-in Git and systemd adapters tag only existing native items. Their
  normal enrichment path performs no I/O; after the documentation delay, a
  bounded and cancellable resolver focuses `git`/`systemctl` help or runtime
  information on the selected item.
- Other commands get a local-man-page fallback for selected native options.
  Declaration-aware extraction prevents an option mention in another entry
  from being mistaken for the selected option. Manual/help text is focused on
  the chosen item and stored in a weighted,
  expiring in-memory cache; the changing fuzzy fragment is not part of the
  cache identity.
- Native path candidates carry a typed filesystem resource separately from
  their label and shell insertion. Configured documentation resolvers receive
  that exact path as one argv entry; they never parse display text or execute
  a shell command string.
- Native shell token context and adapter events are request-scoped. The daemon
  rejects stale events, unknown item IDs, candidate publication by adapters,
  and adapter-event publication by non-adapter peers.
- A Blink.cmp terminal source attaches to the exact live shell by PID. Blink
  renders the shared items and documentation while acceptance is routed back
  to, and acknowledged by, the native shell. Attaching suppresses the terminal
  popup; disconnecting restores it without regenerating candidates.
- A crashed per-shell worker is replaced at the next ZLE session, Fish prompt,
  or Bash prompt. Recovery creates a fresh native session without re-sourcing
  the integration or recapturing its own keybindings.

Diagnostics, snippets, command rewriting, AI completion, history candidates,
Carapace, and fallback filesystem/PATH providers are intentionally outside the
product scope.

## Build

The Rust workspace requires the toolchain declared in `Cargo.toml`.

```sh
cargo build --release
target/release/shell-sense install
```

`install` copies the running executable to `~/.local/bin` and atomically
replaces the application-owned XDG data tree containing the shell integrations,
the Blink source, and the example configuration. Override either destination
with `--bin-dir` or `--data-dir`. User configuration is stored separately and
is never replaced.

During development, each shell entry point also discovers
`target/debug/shell-sense`. Set `SHELL_SENSE_COMMAND` to select an explicit
binary and `SHELL_SENSE_SOCKET` to select an explicit daemon socket.

## Shell initialization

Load Shell Sense after the shell's native completion setup and other line
editor plugins.

Zsh:

```zsh
source "$HOME/dotfiles/shell/shell-sense/shell/zsh/shell-sense.plugin.zsh"
```

Fish:

```fish
source "$HOME/dotfiles/shell/shell-sense/shell/fish/shell-sense.fish"
```

Bash 5.2 or newer:

```bash
source "$HOME/dotfiles/shell/shell-sense/shell/bash/shell-sense.bash"
```

## Compatibility

The supported integration targets are Zsh with its completion system and ZLE,
Fish 4.0 or newer, and Bash 5.2 or newer. The Zsh integration is shell code and
does not embed or require a developer-machine Zsh ABI. It captures calls routed
through the normal `compadd` command. A completion function that explicitly
calls `builtin compadd` bypasses function interception; Zsh currently exposes
no supported hook for that path, and Shell Sense does not modify Zsh's internal
builtin table to work around it.

Fish exposes the strongest arbitrary-buffer completion API. Bash does not
expose an equivalent API or a universal post-edit hook, so continuous Bash
requests reconstruct the public `COMP_*` context. If Readline owns an edit
outside a Shell Sense binding, the next popup action detects the changed
buffer and regenerates native candidates before it can navigate or accept.
Manual Tab remains the exact native fallback on every shell.

ZLE and Fish can safely repaint delayed documentation while their line editor
is otherwise idle. Bash's public Readline API has no equivalent idle callback;
decoding the mailbox from a signal trap can corrupt Readline's stack. Bash
therefore receives delayed documentation on the next Shell Sense action, such
as candidate navigation, documentation scrolling, or documentation toggling.

The CLI prints an initialization line for the discovered XDG data directory:

```sh
shell-sense init zsh
shell-sense init fish
shell-sense init bash
```

Pass `--data-dir` when the assets were installed to an explicit non-XDG path.

## Blink.cmp terminal source

The source module ships in this repository. Add the project to Neovim's
runtime path, register the provider, and select it for terminal mode:

```lua
vim.opt.runtimepath:prepend(vim.fn.expand("~/dotfiles/shell/shell-sense"))

require("blink.cmp").setup({
  keymap = {
    preset = "default",
    ["<C-e>"] = { "select_and_accept" },
  },
  sources = {
    providers = {
      shell_sense = {
        name = "Shell Sense",
        module = "blink-cmp-shell-sense",
        async = true,
      },
    },
  },
  term = {
    enabled = true,
    sources = { default = { "shell_sense" } },
    completion = {
      list = { selection = { auto_insert = false } },
      menu = { auto_show = true },
      ghost_text = { enabled = false },
    },
  },
})
```

The Lua source starts `shell-sense blink` for the terminal job's shell PID.
The bridge waits briefly for a newly opened shell session, sends only
presentation-safe JSON to Neovim, and never applies its LSP-shaped text edit.
The terminal job must be the supported shell process that owns the Shell Sense
session. Blink resolves an item before executing it, so the source tracks the
native generation through both steps. If the shell advances while a selected
item is still visible, an unambiguous equivalent is rebased onto the newest
settled native item; stale documentation work is cancelled silently. The shell
still performs and acknowledges the final edit.

## Defaults

Completion is continuous by default. Closed Tab and Ctrl-Space trigger it
manually. With the popup open, Tab or Ctrl-E accepts, Ctrl-N/Ctrl-P moves one
item, Ctrl-D/Ctrl-U moves one page, and Escape dismisses it. Enter and Ctrl-C
retain the active shell's execution and interrupt semantics. Documentation has
an independent viewport: Ctrl-F/Ctrl-B moves it one page without changing the
selected candidate, and Ctrl-G hides or shows it. The line actions
`documentation-down` and `documentation-up` are also available for custom
bindings.

`popup.scrolloff` controls the minimum number of following candidates kept in
view during navigation and defaults to 2. Near the beginning or end of the
result set, the viewport naturally uses the available rows instead.
`popup.cycle = true` wraps `next` from the last candidate to the first and
`previous` from the first candidate to the last; set it to `false` to clamp at
the endpoints. Selection changes are applied in the shell immediately, while
serial-numbered worker updates cannot overwrite a newer local selection.
While an edit's replacement generation is pending, ZLE keeps the current menu
visible and locally rebases its ghost text when the old predicted line still
matches the new buffer. The new authoritative menu and ghost replace that
continuity frame atomically; incompatible edits simply omit the stale ghost.
ZLE popup redisplays use terminal synchronized-output transactions. Every
ordinary `line-pre-redraw` receives the frame for its current logical
selection. During key repeat, one explicit event-loop redraw is retained and
updated to the latest selection; it cannot depend on ZLE happening to issue a
final redraw after queued input drains. The terminal transaction releases the
result only after ZLE's native redraw.

The popup uses the VS Code/Blink-inspired colors in `config.example.toml`, a
responsive width, no border or selected marker, dimmed detail text, kind icons,
a proportional scrollbar, and a right-side documentation pane. `auto` and
`below` remain available; an explicit `side` falls back below only when the
terminal cannot fit the minimum menu and documentation widths at all.
Side documentation uses `popup.max_rows` so the two surfaces have one aligned
height. Below documentation uses `documentation.max_rows`. Documentation has
its own configurable zero-cell default padding and proportional scrollbar;
its existing line/page actions move that scrollbar without changing the
selected completion.
`documentation.mode = "manual"` starts with the pane hidden and resolves
documentation only after the toggle action opens it; `off` disables the pane
and its actions entirely. `documentation.update_delay_ms` delays only the
replacement document after selection changes: the previous document remains
visible until the selected item's content is ready, so the pane does not flash
closed and open while navigating.

`indicators.file_icons = "filetype"` gives typed file resources a Nerd Font
icon based on their extension. Set it to `"generic"` for one stable file icon.
Directories always use the native completion kind and the folder icon; icon
resolution never probes the filesystem or changes candidate generation.
`indicators.kinds` independently selects `"icon"`, `"text"`, `"both"`, or
`"none"`. Blink continues to render its own kind icons from the semantic item
kind, so this setting applies only to terminal popups.

## Configuration

The default user file is `~/.config/shell-sense/config.toml`. See
`config.example.toml` for the complete version-4 shape. Unknown fields and old
configuration versions are rejected; refactors do not retain compatibility
aliases.

Documentation commands are configured as typed resolver rules. Placeholders
must occupy a complete argv entry; no shell parses or expands them:

```toml
[adapters.documentation]
enabled = true

[[adapters.documentation.resolvers]]
name = "file-information"
kinds = ["file", "symlink"]
command = ["file", "--brief", "--", "$value"]

[[adapters.documentation.resolvers]]
name = "directory-listing"
kinds = ["directory"]
command = ["ls", "-la", "--", "$value"]
```

`$value` is the native provider's exact typed filesystem resource for file,
directory, and symlink items; for other kinds it is the candidate's semantic
label. It must occupy one complete array entry. The first array element is the
executable and every remaining element is passed as one argv entry, so no shell
evaluation, alias/function lookup, or word splitting occurs. The executable is
resolved from the daemon's `PATH`. Resolver output is normalized to plain text;
raw terminal escape sequences are not preserved. For example, an `ls` alias
that expands to `eza` does not affect the rule above. Configure `eza` directly
when that is the desired resolver:

```toml
command = [
  "eza", "--group-directories-first", "--icons=never",
  "--color=never", "-la", "--", "$value",
]
```

Rules are ordered: the first matching configured rule wins. Built-in
command-specific Git/systemd documentation has higher precedence; the generic
man-page option resolver has lower precedence. Resolver output is bounded,
cancellable, and requested only after the documentation delay. The default
rules shown above can be replaced with any read-only command arrays.

For Git refs, the documentation pane intentionally shows the selected ref's
target decoration, short commit and subject, author, and relative date. That
is the same documentation pane—not a second context panel.

Daemon autostart performs a current-protocol handshake. A same-user Shell
Sense daemon that explicitly rejects the current protocol is stopped and
replaced; a socket that does not prove it is Shell Sense is never terminated.

Operational environment variables use `SHELL_SENSE_*`. Typed overrides use a
separate namespace and double underscores for nesting, for example:

```sh
SHELL_SENSE_CONFIG__ACTIVATION__MODE=manual shell-sense config effective
```

Useful commands:

```sh
shell-sense config check
shell-sense config effective
shell-sense config schema
shell-sense config paths
shell-sense doctor
shell-sense doctor --shell fish
```

## Native authority

```text
Zsh  -> _main_complete / compdefs -> Shell Sense rank/UI -> Zsh acceptance
Fish -> complete -C               -> Shell Sense rank/UI -> commandline
Bash -> programmable completion   -> Shell Sense rank/UI -> READLINE_LINE
```

An exact native request runs first. After an exact miss, a structurally
broadened request can remove only the active fuzzy fragment while preserving
context such as a path prefix, `--`, or `--option=`. Frizbee then ranks the
native result set. Shell Sense never manufactures a candidate to make a fuzzy
query succeed.

## Verification

```sh
cargo test --workspace --all-targets
cargo clippy --workspace --all-targets -- -D warnings
cargo bench -p sense-rank --bench rank
cargo build --release
zsh tests/native-conformance.zsh
zsh tests/fifo-transport.zsh
zsh tests/live-client.zsh
zsh tests/live-fish.zsh
zsh tests/live-bash.zsh
zsh tests/live-blink.zsh
zsh tests/release-latency.zsh
zsh tests/release-package.zsh
```

The PTY tests verify real line-editor behavior and native candidate acceptance;
the shared 10-case provider contract covers fuzzy subcommands, short, combined,
long and value options, user completions, path forms, programmable completions,
and context reconstruction. The release gates enforce a 30 ms p95
request-to-worker-delivery budget for the standard completion fixture, retain a
separate 75 ms terminal-observation guard, and validate the exact installed
binary, runtime assets, shell initialization, and Blink module.
