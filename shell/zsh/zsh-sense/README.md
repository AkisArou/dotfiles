# zsh-sense

`zsh-sense` is a greenfield Rust IntelliSense system for Zsh. Zsh remains the
source of valid completions and the owner of terminal editing; a persistent
Rust daemon adds continuous invocation, structured ranking, documentation,
snippets, diagnostics, context adapters, and presentation.

The product architecture and full feature inventory live in [PLAN.md](PLAN.md).
The checked-in [config.example.toml](config.example.toml) is parsed by the same
typed model that generates the JSON Schema.

## Current implementation

The first interactive product slice is active in this dotfiles repository. It
includes:

- an LSP-inspired, raw-byte-safe completion/intelligence model;
- bounded, length-prefixed MessagePack messages over Unix sockets;
- daemon sessions, attached workers, generations, streaming, cancellation,
  role authorization, and per-source request completion;
- continuous-by-default typed configuration, state-specific keybindings,
  named profiles, strict validation, JSON Schema generation, and external
  adapter configuration;
- in-process provider/context-adapter contracts with capability, authority,
  deadline, cancellation, and bounded-output declarations;
- a language-neutral, versioned external adapter manifest and wire contract;
- portable Zsh probing and content-addressed ABI keys;
- allowlisted bindgen generation against an explicitly configured Zsh source;
- a panic-contained Rust `cdylib` skeleton that has been load-tested as the
  Zsh module `sense/core` against Zsh 5.9.2;
- Frizbee ranking with adaptive typo tolerance, match spans, group-preserving
  order, bounded candidate views, stable selection, and cross-source
  deduplication/enrichment;
- request-scoped Zsh capture state with raw-byte insertion, opaque acceptance
  identities, stale-generation rejection, and terminal-safe display data;
- a permanent portable `compadd` backend with PTY coverage for descriptions,
  groups, explanations, and acceptance;
- daemon aggregation that converts streamed source batches into revisioned,
  ranked `CandidateView` updates;
- a persistent per-shell Rust bridge with separate ZLE-client and attached
  completion-worker daemon roles, bounded binary-safe shell framing, negotiated
  candidate-batch splitting, and an end-to-end request/capture/rank/selection
  integration test;
- a live ZLE client using private, close-on-exec FIFOs watched by `zle -F`;
- continuous completion by default, plus manual Tab activation and configurable
  state-specific keybindings;
- a persistent, ZLE-owned VS Code-inspired panel with descriptions, optional
  kind indicators, selection, item navigation, page navigation, and a
  terminal-clamped width derived from the full ranked result set;
- styled end-of-line ghost text for complete, unique authoritative prefix
  matches, with ambiguity suppression and configurable token/word/path-segment
  acceptance; full completion tokens are still accepted through Zsh;
- fuzzy candidate generation and Frizbee ranking, including typo matches such
  as `systemctl rstart` -> `restart` and path matches such as `cd dfil` ->
  `dotfiles/`;
- adaptive candidate broadening: one- and two-character fragments stay on
  Zsh's bounded prefix path, while fuzzy subsequence generation starts at a
  configurable fragment length;
- correct Backspace regeneration and automatic nested completion after
  accepting a directory;
- Zsh-owned acceptance, preserving completion quoting, prefixes, suffixes, and
  directory behavior;
- CLI foundations for the daemon, configuration, environment diagnostics, and
  shell initialization output, including the persistent `worker` command.

The plugin is sourced at the end of this repository's `.zshrc`, after compinit
and the other ZLE plugins. `fzf-tab` remains commented out. The release binary
is preferred over a debug binary.

This is not the whole product described in `PLAN.md` yet. Native `compadd`
interception, isolated completion capture, cached refiltering, documentation
panes, snippets, semantic adapters, history-backed continuations, and
diagnostics remain on the roadmap. Portable capture currently calls `_main_complete`
synchronously in the live shell, so an intrinsically slow third-party
completion function can still pause a capture once it has started. It will not
start while ZLE already has input queued, so ordinary pending edits win.

## Configuration

Defaults are built in. To customize them, copy `config.example.toml` to
`$XDG_CONFIG_HOME/zsh-sense/config.toml` (normally
`~/.config/zsh-sense/config.toml`) and edit it. The default interaction is:

- completion opens continuously after edits;
- Tab opens completion when closed and accepts when open;
- Ctrl-N/Ctrl-P select the next/previous item;
- Ctrl-D/Ctrl-U page down/up;
- Ctrl-E accepts the selected item;
- Enter executes the current command and removes the popup first;
- Ctrl-C removes the popup before interrupting the current command line;
- Escape dismisses the popup.

For the Ctrl-C action, zsh-sense temporarily routes `^C` through ZLE only
while the prompt is active and restores the terminal's ordinary SIGINT
character before executing a command.

Set `activation.mode = "manual"` for Tab-only completion, or use `"hybrid"`
and `"disabled"` for the other supported policies. `activation.debounce_ms`,
event/character triggers, popup dimensions/decorations, descriptions,
indicator mode, and all state-specific keybindings are typed configuration.
`ghost_text.enabled`, `ghost_text.source`, `ghost_text.minimum_confidence`, and
`ghost_text.partial_accept` control completion-derived inline suggestions.
Ghost text is deliberately limited to end-of-line, where ZLE can render it
without mutating the editable buffer; ambiguous, incomplete, fuzzy-only, and
truncated candidate sets do not produce it.
The popup grows to its contents between `popup.min_width` and
`popup.max_width`; set both values to the same number for a fixed-width panel.
The default popup is borderless and markerless like this repository's BlinkCmp
configuration; both decorations remain configurable. Its palette resolves the
BlinkCmp highlights and PmenuSel override from this dotfiles repository's
`vscode.nvim` configuration. Menu, border, selection, label, fuzzy-match,
detail, kind, footer, scrollbar, and per-kind colors can all be changed under
`[styles]`; each value is one ZLE highlight specification.
`sources.zsh.fuzzy_min_query_chars` controls when the portable source broadens
the candidate universe (default `3`); this is separate from Rust's final fuzzy
ranking and typo policy.
Run `zsh-sense config check` before reloading a changed file.

## Development

```sh
cargo test --workspace --all-targets
cargo clippy --workspace --all-targets -- -D warnings
zsh tests/portable-capture.zsh
zsh tests/fifo-transport.zsh
zsh tests/live-client.zsh
zsh tests/user-config-smoke.zsh
cargo run -- config check --path config.example.toml
cargo run -- doctor --config config.example.toml
```

Generate the editor schema with:

```sh
cargo run -- config schema
```

Native bindings are opt-in and require a configured Zsh source/build tree with
generated module headers:

```sh
SENSE_ZSH_SOURCE=/path/to/zsh-source \
SENSE_ZSH_BUILD=/path/to/zsh-build \
  cargo build -p sense-zsh-module --features native-module
```

Ordinary CLI/daemon builds do not execute or embed a build-machine Zsh probe.
Only a native-module build receives an exact Zsh identity. Installation will
select/cache modules by that ABI key and run an isolated load test before
activation, so users will not need the developers' Zsh version.
