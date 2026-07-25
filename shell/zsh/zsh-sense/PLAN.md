# zsh-sense: Product and Implementation Plan

Status: accepted architecture; implementation active  
Last updated: 2026-07-25

## Implementation status

The first greenfield foundation is implemented and verified. This status is
deliberately narrower than the feature checklist: a feature remains unchecked
until it works end to end in interactive ZLE.

Implemented now:

- final Cargo workspace foundations for model, protocol, configuration,
  daemon, CLI, provider API, adapter API, Zsh ABI probing, allowlisted Zsh
  bindings, and the native Rust module;
- raw-byte-safe LSP-inspired completion, documentation, signature,
  diagnostic, action, preview, and ghost-text contracts;
- bounded length-prefixed MessagePack framing, sessions, attached worker
  roles, generations, cancellation, streaming candidate batches, source-aware
  completion, and role authorization over a mode-0600 Unix socket;
- continuous-by-default configuration, manual/Tab fallback, state-specific
  keybindings, display/icon/documentation controls, source/adapter limits,
  ordered rules, named profile layering, strict unknown-field rejection,
  cross-field validation, JSON Schema generation, and a checked example;
- cancellable in-process provider/context-adapter API and a language-neutral
  external adapter manifest/wire contract with capabilities, authority,
  selectors, trust/side-effect declarations, deadlines, concurrency, and
  output bounds;
- runtime Zsh identity probing and content-addressed ABI keys separated from
  build-specific `sense-zsh-sys` bindings, preserving portable/cross-build
  release binaries;
- allowlisted bindgen generation tested against freshly configured Zsh 5.9.2
  sources, plus a panic-contained Rust `cdylib` skeleton successfully
  load-tested and unloaded as `sense/core` in Zsh 5.9.2;
- CLI foundations for `daemon`, `config check/effective/schema/paths`,
  `doctor`, named profiles, and initialization output;
- Frizbee 0.11 ranking with the `safe_read` feature, adaptive typo tolerance,
  exact/prefix/context boosts, group-preserving ordering, bounded views,
  cross-source metadata enrichment/deduplication, match spans, and stable
  selection across streamed updates;
- request-scoped native/portable capture records with raw insertion bytes,
  complete backend-neutral insertion metadata, opaque acceptance routing,
  stale-generation rejection, strict count/byte bounds, and terminal-control
  sanitization;
- a documented portable `compadd` capture backend, exercised through a real
  ZLE/zpty test for descriptions, explanations, groups, and Zsh-owned
  acceptance;
- daemon-side aggregation into revisioned `CandidateView` updates, with the
  configured source priorities and `matching.max_results` applied;
- persistent per-shell Rust bridge using distinct ZLE-client and attached
  completion-worker daemon roles; bounded binary-safe netstring framing for
  Zsh, strict typed command decoding, request-scoped capture assembly,
  negotiated MessagePack batch splitting, ranked view streaming, and opaque
  Zsh acceptance routing. The full request/capture/rank/select path is covered
  against a real daemon;
- a live ZLE client over private, immediately unlinked, close-on-exec FIFOs,
  including continuous/manual activation, original-widget delegation,
  configurable Tab/Ctrl keybindings, cancellation, stale-generation defense,
  descriptions, item kinds, a bordered list, item/page navigation, and
  Zsh-owned acceptance;
- fuzzy Zsh candidate generation through `_main_complete`'s documented
  `matcher-list` policy, adaptive short-fragment candidate bounding, Frizbee
  ranking, path-component-aware filtering, and post-accept path chaining
  (`cd dfil` -> `dotfiles/` -> nested candidates);
- interactive PTY coverage for continuous completion, manual Tab, Backspace,
  ordinary Space/Backspace input responsiveness, flag descriptions, fuzzy typo
  matching, scrolling, path semantics, FIFO framing, and the complete real
  `.zshrc` plugin stack;
- release-mode activation from the user's `.zshrc`, with `fzf-tab` kept
  disabled.

Still to build: real native `compadd` interception, an isolated/asynchronous
Zsh capture path, live config reload, full provider scheduling, syntax parsing,
cached local refiltering, snippets, rich style-span/layout support,
documentation panes, history, ghost text, and semantic adapter
implementations. The current portable backend invokes `_main_complete`
synchronously in the live ZLE process. It bounds short-query matching and
declines to start when input is already pending, but a slow third-party
completion function can still delay an individual capture once it starts,
until isolated capture lands. The obsolete C prototype and its shell runtime
have been removed; there is no legacy fallback in the workspace.

## 1. Product statement

`zsh-sense` will be a continuous, non-modal IntelliSense system for Zsh. It
will use Zsh's existing completion system as the universal source of valid
shell completions, then add editor-style ranking, presentation,
documentation, signatures, snippets, diagnostics, history, and optional
runtime context adapters.

The product is a greenfield Rust implementation. The existing C prototype is
not a runtime dependency, migration layer, fallback, compatibility target, or
source-level starting point. The final repository will not retain a legacy C
implementation.

The default experience is continuous completion. Manual completion remains a
first-class mode, and Tab can be configured to trigger completion when the
popup is closed and accept or navigate when it is open.

## 2. Non-negotiable design decisions

1. ZLE remains the only owner of terminal input, the editable command buffer,
   keymaps, history, cursor movement, and redraw.
2. The persistent Rust daemon never reads from or writes directly to the
   terminal.
3. Existing Zsh completion functions remain the universal candidate source.
   We do not replace `_main_complete`, `_git`, `_systemctl`, or user `compdef`s.
4. Zsh-generated candidates are accepted through Zsh's completion machinery,
   not pasted as plain strings.
5. The native Zsh module is deliberately tiny, single-threaded, version-aware,
   and contains no fuzzy matcher, database, parser, asynchronous runtime, or
   context adapter.
6. Expensive and failure-prone work lives in the crash-isolated daemon or an
   adapter process.
7. Context adapters are part of the architecture from the beginning, but they
   enrich completion rather than replacing ordinary Zsh completion.
8. The internal completion model is inspired by and convertible to LSP, but
   the latency-sensitive client/daemon protocol is not the standard LSP wire
   protocol.
9. VS Code JSONC snippet files and TextMate/LSP snippet body syntax are the
   user-facing snippet standards.
10. Configuration is validated, discoverable, reloadable, and never requires
    editing plugin source.
11. No user must install the same Zsh version used by the developers. The
    installer selects, downloads, or locally builds the small native adapter
    for the user's Zsh. A portable backend remains available when a native
    adapter cannot yet be installed.
12. `fzf`, a standalone TUI, and raw ANSI escape sequences are not part of the
    continuous completion path.

## 3. Architecture

```text
                              terminal
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│ Thin ZLE client                                                 │
│                                                                 │
│ BUFFER/CURSOR · keymaps · widgets · POSTDISPLAY                 │
│ region_highlight · acceptance · snippet field navigation        │
└──────────────┬──────────────────────────────┬───────────────────┘
               │                              │
               │ live shell state             │ nonblocking socket
               ▼                              ▼
┌───────────────────────────────┐  ┌──────────────────────────────┐
│ Isolated Zsh completion worker│  │ Persistent Rust daemon       │
│                               │  │                              │
│ `_main_complete`              │  │ session/request scheduler    │
│ exact native adapter or       │  │ parser and command context   │
│ portable capture backend      │  │ source/adapter aggregation   │
│ compadd metadata capture      │  │ Frizbee ranking              │
└──────────────┬────────────────┘  │ cache, history, documentation│
               │ candidate batches │ diagnostics and layout       │
               └──────────────────▶│                              │
                                  └───────────┬──────────────────┘
                                              │
                           ┌──────────────────┼──────────────────┐
                           ▼                  ▼                  ▼
                     built-in source    built-in context   external adapter
                     and enrichers      adapters           processes
```

### 3.1 ZLE client

The Zsh layer is intentionally small. It will:

- observe edits and cursor/context changes;
- implement activation and keybinding behavior;
- start an isolated completion request with live shell state;
- watch the daemon connection with `zle -F`;
- ignore stale generations;
- render plain text and style spans through `POSTDISPLAY` and
  `region_highlight`;
- apply typed edits, Zsh completion selections, ghost-text acceptance, and
  snippet placeholder navigation;
- degrade cleanly to normal ZLE behavior if the daemon is unavailable.

It will not rank candidates, parse shell syntax, query Git/systemd, parse
documentation, or maintain a database.

### 3.2 Native adapter

The native adapter is a Rust `cdylib` loaded by Zsh. Its responsibilities are:

- observe the real `compadd` and description paths, including calls that
  bypass shell function wrappers;
- encode Zsh match metadata and opaque insertion identity;
- expose the minimum operations needed to capture and later select a match;
- verify that it was built for a compatible Zsh ABI key;
- catch every Rust panic at every C ABI entry point;
- start no threads and initialize no asynchronous runtime.

All raw bindings are private to the internal `sense-zsh-sys` crate.

### 3.3 Daemon

The daemon is one process per user with multiple shell sessions. It owns:

- request generations, cancellation, deadlines, backpressure, and streaming;
- incremental Zsh parsing and semantic command context;
- candidate source and context-adapter scheduling;
- cache invalidation through context epochs;
- Frizbee matching and stable multi-source ranking;
- frecency and history persistence;
- documentation and schema caches;
- diagnostics, code actions, previews, and responsive layout;
- config loading, validation, live reload, and adapter supervision.

### 3.4 Context adapters

Context adapters are designed in from the first provider API. They add dynamic
meaning that ordinary completion cannot express, such as a branch's last
commit or a service's current state.

Adapters have these independent capabilities:

```text
complete     contribute additional candidates
enrich       attach detail/kind/tags to existing candidates
resolve      provide documentation for a selected item
signature    provide current command signature/argument information
diagnose     provide authoritative diagnostics
actions      provide deterministic fixes/actions
preview      provide a factual selected-item preview
```

Every adapter declares:

- command/context selectors;
- supported capabilities;
- whether it is authoritative, partial, or advisory;
- triggers and cache dependencies;
- soft and hard deadlines;
- side-effect/trust requirements;
- output and concurrency limits;
- cancellation support.

Built-in adapters run as daemon components. Third-party adapters run out of
process; no third-party dynamic library is loaded into Zsh or the daemon.

Initial built-in adapters are planned for Git and systemd. Cargo/npm project
metadata, Docker, process, and other adapters follow only where their rich
features cannot be obtained generically.

## 4. Request lifecycle

1. A meaningful edit creates generation `N`.
2. The activation policy decides whether to request automatically, debounce,
   or wait for a manual trigger.
3. The ZLE client sends the buffer, cursor, cwd, dimensions, session/context
   epoch, and selectively permitted environment context to the daemon.
4. The daemon incrementally parses the line and starts cheap sources.
5. An isolated child of the live Zsh session invokes `_main_complete`; its
   adapter streams Zsh candidates to the daemon.
6. Cached candidates may be refiltered immediately while slower sources run.
7. Context adapters enrich or add items according to their latency budgets.
8. The daemon merges, deduplicates, ranks, lays out, and streams view updates.
9. ZLE applies an update only if its generation, buffer fingerprint, and
   context epoch still match.
10. Selection changes can issue a lazy `resolve` or `preview` request.
11. Acceptance dispatches by insertion strategy:
    - `ZshMatch`: rerun/select through live Zsh completion;
    - `TextEdit`: validate and apply an explicit range edit;
    - `Snippet`: apply the edit and create a placeholder session.
12. Acceptance of a structural item can immediately trigger the next context.

No source or adapter work is allowed to block a keypress.

## 5. Relationship to LSP

### 5.1 What we reuse

The core item model intentionally follows LSP concepts:

- label and label details;
- kind and tags;
- short detail versus long documentation;
- filter and sort text;
- insert/replace edits;
- commit characters;
- preselection;
- lazy completion-item resolution;
- incomplete candidate lists;
- snippet insert format;
- signature help;
- diagnostics and code actions.

We will maintain explicit conversions between `SenseCompletionItem` and LSP
completion items where semantics overlap. A future `sense-adapter-lsp` can
consume or expose an LSP endpoint without changing the internal model.

### 5.2 Why the core protocol is not LSP

Standard LSP assumes versioned text documents identified by URIs and a
request/response completion operation. A shell command line instead has:

- a short ephemeral buffer tied to a live shell process;
- cwd, environment, aliases, functions, options, styles, and completion state;
- non-UTF-8 path and argument bytes;
- Zsh-specific opaque insertion semantics;
- completion groups and source provenance;
- multiple providers streaming partial updates independently;
- terminal dimensions, popup selection, scrolling, and render spans;
- context epochs and post-accept chaining.

Encoding all of this as custom LSP extensions would retain JSON-RPC overhead
without providing interoperability with existing language servers. Standard
LSP completion also does not stream independent partial completion lists.

The internal protocol will therefore use bounded, length-prefixed MessagePack
over Unix-domain sockets, with LSP-inspired messages and data types. An LSP
bridge is an adapter, not the hot path.

### 5.3 Zsh-to-worker transport

The interactive shell cannot encode MessagePack natively, and shell words may
contain newlines or bytes that are not valid UTF-8. Each interactive Zsh
therefore owns one persistent Rust `worker` process. Its standard streams use a
small binary-safe protocol made from netstrings:

```text
netstring(command) + netstring(field-count) + N × netstring(raw-field)
```

Commands are lowercase ASCII identifiers; fields are opaque bytes unless the
command schema declares them as canonical decimal integers or UTF-8 display
metadata. Both decoding and encoding enforce field-count, per-field, and
whole-message limits. Framing is incremental, so a `zle -F` callback can feed
arbitrary read chunks without line parsing or Base64. No candidate creates a
subprocess.

The worker holds two daemon connections. A `ZleClient` connection creates and
owns the session; a `CompletionWorker` connection attaches to that session and
publishes captured Zsh batches. The daemon's advertised maximum frame size is
honored by splitting batches while retaining the final/incomplete markers.
Selection returns only an opaque live capture route; display text is never
used to reconstruct insertion.

## 6. Core data model

```rust
struct SenseCompletionItem {
    id: ItemId,
    source: SourceId,
    label: String,
    label_detail: Option<String>,
    filter_text: String,
    sort_text: Option<String>,
    kind: CompletionKind,
    tags: ItemTags,
    detail: Option<String>,
    documentation: DocumentationState,
    group: Option<GroupId>,
    edit: TextEdit,
    insertion: InsertStrategy,
    commit_characters: Vec<char>,
    original_order: u32,
    provider_relevance: i32,
    confidence: Confidence,
    capabilities: ItemCapabilities,
    opaque_data: ProviderData,
}

enum InsertStrategy {
    ZshMatch { fingerprint: ZshMatchFingerprint },
    TextEdit,
    Snippet { syntax: SnippetSyntax },
}
```

Completion items, signature help, diagnostics, code actions, previews, and
ghost text are separate models. A short completion description is not used as
a container for every feature.

### 6.1 Byte and character correctness

Unix arguments and filenames are not guaranteed to be UTF-8. The protocol and
model distinguish:

- raw insertion bytes or an opaque Zsh match fingerprint;
- valid UTF-8 UI labels, with escaped display for invalid bytes;
- UTF-8 byte edit ranges;
- grapheme boundaries for editing UX;
- terminal cell widths for rendering.

Candidate insertion must never be reconstructed from a lossy display label.

## 7. Activation and keybinding model

### 7.1 Activation modes

```text
continuous  automatic after configured edits; manual trigger also works
manual      only an explicit trigger opens or refreshes completion
hybrid      automatic only in selected contexts; manual elsewhere
disabled    plugin leaves ordinary ZLE completion untouched
```

`continuous` is the default.

Tab is state-sensitive and configurable. The recommended defaults are:

```text
popup closed     Tab triggers immediately
popup open       Tab accepts the selected item
snippet active   Tab advances to the next placeholder
```

Users can instead make Tab navigate, pass through to the original widget, or
do nothing in any state.

### 7.2 Trigger categories

The user can configure:

- edit events: insert, backspace, delete, word-delete, paste, history change,
  cursor movement, and acceptance;
- structural characters such as `/`, `-`, `=`, `:`, and space;
- minimum query length by completion context;
- contexts that may open on an empty query, such as options, paths, and
  subcommands;
- base debounce milliseconds and immediate-trigger cases;
- command/cwd/keymap rules that force continuous, hybrid, manual, or disabled
  mode;
- suppression while searching, selecting a region, or using vi command mode.

## 8. Configuration design

### 8.1 Principles

- One user-facing TOML file at `$XDG_CONFIG_HOME/zsh-sense/config.toml`.
- A `version` field for deliberate config migrations.
- Typed deserialization with unknown keys rejected by default.
- JSON Schema generated from the same Rust configuration structs with
  `schemars`, enabling editor completion and documentation.
- Layering through Figment: built-in defaults, selected profile, user file,
  and explicit environment/CLI overrides.
- Live reload through `notify`; a malformed replacement is rejected atomically
  and the previous valid configuration remains active.
- `zsh-sense config check`, `config explain`, `config schema`, and
  `config effective` commands.
- Per-command and per-cwd rules with deterministic ordered merging.
- Project-local configuration is disabled unless the project is explicitly
  trusted.

Named profiles are ordinary partial TOML files at
`$XDG_CONFIG_HOME/zsh-sense/profiles/<name>.toml`. The selected profile layers
between built-in defaults and the main user file, so explicit values in
`config.toml` still win. Selection precedence is CLI `--profile`, then
`ZSH_SENSE_PROFILE`, then the main file's `profile` key. Profile names are
path-safe identifiers; they cannot escape the profiles directory.

### 8.2 Illustrative default configuration

The exact names may be refined before the config schema is frozen, but all of
these controls are intentional product requirements.

```toml
version = 1
profile = "default"

[activation]
mode = "continuous"             # continuous | manual | hybrid | disabled
debounce_ms = 35
max_debounce_ms = 75
min_query_chars = 1
trigger_on_empty = ["options", "paths", "subcommands"]
events = [
  "insert", "backspace", "delete", "word-delete",
  "paste", "history", "cursor", "accept"
]
characters = ["/", "-", "=", ":", " "]
immediate_characters = ["/", "-", "="]
after_accept = true

[keybindings.closed]
tab = "trigger"
"ctrl-space" = "trigger"

[keybindings.popup]
tab = "accept"
"ctrl-e" = "accept"
"ctrl-n" = "next"
"ctrl-p" = "previous"
"ctrl-d" = "page-down"
"ctrl-u" = "page-up"
escape = "dismiss"
right = "accept-next-token"
end = "accept-ghost"

[keybindings.snippet]
tab = "next-placeholder"
"shift-tab" = "previous-placeholder"
escape = "cancel-snippet"

[matching]
engine = "frizbee"
case = "smart"                  # smart | sensitive | insensitive
typos = "adaptive"              # off | adaptive | fixed
max_typos = 2
typo_min_query_chars = 4
max_results = 1000              # source caches retain the rest for refiltering
preserve_groups = true
use_frecency = true
use_project_proximity = true
explain_scores = false

[popup]
enabled = true
decorations = "full"            # full | minimal | none
border = "rounded"              # rounded | sharp | ascii | none
title = true
footer = true
scrollbar = true
group_headings = true
descriptions = true
max_rows = 10
max_width = 140
min_width = 24
padding = 1

[indicators]
kinds = "icon"                  # icon | text | both | none
icon_theme = "nerd-font"        # nerd-font | unicode | ascii
file_icons = "devicons"         # devicons | generic | none
selected_marker = "›"

[documentation]
mode = "auto"                   # auto | side | below | manual | off
resolve_delay_ms = 80
side_min_columns = 100
width_ratio = 0.45
max_rows = 14
render_markdown = true
sources = ["completion", "schema", "man", "adapter", "help"]

[ghost_text]
enabled = true
source = "best"                 # best | history | completion
minimum_confidence = 0.82
at_end_only = true
partial_accept = "token"        # token | word | path-segment | off

[sources.zsh]
enabled = true
priority = 100
soft_timeout_ms = 100
hard_timeout_ms = 1000
max_candidates = 100000
candidate_filter = "subsequence"
fuzzy_min_query_chars = 3

[sources.filesystem]
enabled = true
priority = 80
respect_hidden_prefix = true

[sources.history]
enabled = true
priority = 40

[sources.snippets]
enabled = true
priority = 35

[adapters]
enabled = true
allow_external = true
default_soft_timeout_ms = 80
default_hard_timeout_ms = 500
maximum_concurrency = 4

[adapters.git]
enabled = true
preview = true

[adapters.systemd]
enabled = true
preview = true

[snippets]
enabled = true
format = "vscode"
paths = ["~/.config/zsh-sense/snippets/*.code-snippets"]
project_snippets = "trusted-only"
linked_placeholders = true
choices = true
transforms = true
sort = "inline"                 # top | inline | bottom | none

[history]
enabled = true
incognito = false
database = "~/.local/state/zsh-sense/history.sqlite3"
learn_only_successful = true
maximum_age_days = 365
sensitive_patterns = ["*password*", "*token*", "*secret*", "*Authorization:*"]

[diagnostics]
enabled = true
minimum_confidence = "authoritative"
show_in_popup = true
underline_buffer = true

[safety]
enabled = true
confirmation = "critical-only"  # off | critical-only | configured
ai_may_block_execution = false

[cache]
memory_mib = 128
candidate_ttl_seconds = 30
documentation_ttl_seconds = 3600
persist_warm_metadata = true

[native]
mode = "auto"                   # auto | native | portable
portable_fallback = true
auto_rebuild = true
allow_signed_prebuilt = true

[logging]
level = "warn"
file = "~/.local/state/zsh-sense/zsh-sense.log"
include_command_lines = false

[styles]
border = "fg=#569cd6"
selected = "fg=#ffffff,bg=#264f78"
label = "fg=#dcdcaa"
detail = "fg=#9da5b4"
group = "fg=#4ec9b0"
footer = "fg=#808080"
diagnostic_error = "fg=#f14c4c,underline"
diagnostic_warning = "fg=#cca700,underline"
ghost = "fg=#606060"

[[rules]]
name = "manual completion for sensitive tools"

[rules.match]
commands = ["gpg", "pass", "passwd"]

[rules.activation]
mode = "manual"

[rules.history]
enabled = false

[[rules]]
name = "minimal remote UI"

[rules.match]
environment = { SSH_CONNECTION = "*" }

[rules.popup]
decorations = "minimal"
max_rows = 6

[rules.indicators]
kinds = "text"
file_icons = "none"
```

The real generated configuration reference will document defaults, accepted
values, merging behavior, and whether an option is live-reloadable.

## 9. Context adapter protocol

Third-party adapters are discovered through manifests rather than dynamic
libraries. A manifest includes:

```text
id and semantic version
executable and protocol version
command/context selectors
capabilities
configuration schema
trust/side-effect declaration
default deadlines and resource limits
```

The adapter protocol uses request IDs, cancellation, bounded messages, and the
same LSP-convertible item models. External adapters can be written in any
language. A small Rust SDK will be provided, but it is not required.

Adapters do not receive the entire environment by default. They receive an
allowlisted context. Project-local adapters require explicit trust.

Planned built-in adapter implementations:

- Git: repository discovery, refs, worktrees, commit summaries, branch state,
  ahead/behind information, and lazy previews;
- systemd: unit state and factual status through D-Bus where available;
- Cargo: workspace packages, features, targets, examples, benches, and tests;
- npm: workspace scripts and package ownership from declarative metadata;
- Docker/Podman: container and image metadata through the daemon API where
  configured;
- process: PID/name/resource context for process-oriented completions.

These adapters supplement Zsh candidates. They do not need to regenerate
candidate names that Zsh already supplied.

## 10. Snippets

### 10.1 File format

`zsh-sense` will read VS Code `*.code-snippets` JSONC files. Each named snippet
supports VS Code-compatible `prefix`, `body`, `description`, and `scope`
fields. Shell-specific conditions can be supplied in a separate zsh-sense TOML
metadata file so ordinary VS Code snippet files remain reusable.

Snippet bodies use the TextMate/LSP syntax:

```text
$1, $2, $0
${1:default}
${1|one,two,three|}
linked placeholders
variables with defaults
escaped metacharacters
placeholder and variable transforms
```

### 10.2 Library decision

No currently identified focused crate meets all requirements with an
appropriate maturity/license/scope balance:

- `editor-core` contains a useful subset but brings a complete editor engine;
- `vix-snippet-tool` is intentionally a smaller subset;
- `kopitiam-snippet` is very new, omits transforms, and is GPL-3.0-only;
- the other discovered crates are editor-internal or incomplete subsets.

We will therefore create a small independent `sense-snippet` crate based on
the published LSP/VS Code grammar, using `winnow` for parsing, `regex` for
transforms, and `jsonc-parser` for VS Code snippet files. This is justified
custom code rather than reimplementing an available production library. It
will be property-tested and fuzzed independently.

### 10.3 ZLE snippet session

The ZLE client tracks active placeholder ranges after insertion. It supports:

- next/previous placeholder navigation;
- active-placeholder selection/highlighting;
- linked placeholder updates;
- choice placeholders through the existing completion popup;
- multiline snippets;
- correct range shifts after edits;
- one coherent undo transaction where ZLE permits it;
- cancellation after incompatible edits or command execution.

## 11. Zsh ABI compatibility and distribution

### 11.1 Direct answer

A native module compiled against Zsh 5.9.2 internals must not be assumed safe
to load into a different Zsh build. Zsh installs binary modules in
version-specific locations and does not promise a stable private completion
ABI.

This does **not** mean users must install the developers' Zsh version. Only the
small adapter is build-specific. The daemon, configuration, schemas, history,
ranking, adapters, and UI protocol are not tied to that Zsh version.

### 11.2 ABI key

The adapter cache key includes at least:

```text
target triple
pointer width and endianness
ZSH_VERSION
ZSH_PATCHLEVEL when available
zsh-sense native ABI revision
relevant compile/configure feature fingerprint
```

`ZSH_PATCHLEVEL` identifies the source revision used to build Zsh more
precisely than the release number.

### 11.3 Installation flow

`zsh-sense install` and package-manager hooks will:

1. probe the user's actual Zsh executable, version, patchlevel, module suffix,
   and dynamic-module capability;
2. look for an already cached compatible adapter;
3. use a signed/checksummed prebuilt adapter for common official release and
   platform combinations when allowed;
4. otherwise build the adapter locally against verified matching Zsh sources
   or installed build metadata;
5. run an isolated load/capture self-test before making it active;
6. cache it by ABI key, never overwrite another version's adapter;
7. use the portable capture backend if no verified native adapter is ready.

After a Zsh upgrade, the old module is never loaded speculatively. The daemon
can rebuild the new adapter in the background when local sources/tooling are
available. Network downloads occur only under the signed-prebuilt policy and
are reported by installation/update commands.

### 11.4 Portable backend

The portable backend uses documented Zsh completion widgets and a temporary
shell-level `compadd` capture. It is less complete because an explicit
`builtin compadd` can bypass it, but it provides continuous/manual completion,
ranking, UI, schemas, snippets, history, and adapters while the native adapter
is unavailable.

Configuration modes:

```text
auto      prefer a verified native adapter, otherwise portable
native    require native fidelity and report an actionable error otherwise
portable  never load a native module
```

### 11.5 `sense-zsh-sys`

We will not depend directly on the published `zsh-sys` as a foundational
crate. The internal `sense-zsh-sys` crate will:

- obtain an explicitly selected and checksummed Zsh source;
- run the required configuration/header generation;
- use bindgen allowlists for only the required types, globals, and functions;
- hide every raw binding from the rest of the workspace;
- generate layout/version assertions;
- expose safe-ish, narrow operations to `sense-zsh-module`;
- permit a microscopic new C preprocessor shim only if bindgen cannot express
  a required macro. Such a shim contains no product or completion logic.

## 12. Dependency audit

Versions below are the researched candidates as of 2026-07-25. Final versions
will be pinned in `Cargo.lock` after API, license, security, and benchmark
review. Default features will be disabled where they pull unused code.

### 12.1 Adopt for the core

| Library | Planned use | Decision |
|---|---|---|
| `tokio` 1.53 | daemon runtime, Unix sockets, processes, timers | adopt in daemon only |
| `tokio-util` 0.7 | cancellation tokens and length-delimited framing | adopt |
| `serde` 1.0 | model/config/protocol serialization | adopt |
| `rmp-serde` 1.3 | compact internal MessagePack protocol | adopt |
| `bytes` 1.12 | bounded frame buffers | adopt |
| `frizbee` 0.11 | fuzzy/typo matching and highlight indices | adopt in daemon; start with `safe_read` |
| `tree-sitter` 0.26 | incremental parsing | adopt |
| `tree-sitter-zsh` | pinned Zsh grammar | adopt at audited revision |
| `figment` 0.10 | layered typed configuration | adopt with TOML/env features only |
| `toml` 1.1 | TOML parsing/serialization | adopt |
| `schemars` 1.2 | generated config/manifest JSON Schema | adopt |
| `notify` 8.2 | stable config/snippet/schema watch events | adopt stable release |
| `notify-debouncer-mini` 0.7 | coalesce editor save event bursts | adopt if needed after measurement |
| `arc-swap` 1.9 | atomic active-configuration replacement | adopt |
| `etcetera` 0.11 | XDG config/cache/state/runtime locations | adopt |
| `moka` 0.12 | weighted TTL/TinyLFU daemon caches | adopt with `future` feature |
| `rusqlite` 0.40 | local frecency/history database | adopt |
| `tokio-rusqlite` 0.7 | serialized async access to SQLite | adopt after focused audit |
| `unicode-width` 0.2 | terminal cell widths | adopt |
| `unicode-segmentation` 1.13 | grapheme/word boundaries | adopt |
| `text-size` 1.1 | strongly typed UTF-8 byte offsets/ranges | adopt |
| `bstr` 1.13 | non-UTF-8 Unix candidate/path data | adopt |
| `devicons` 0.6 | optional file icons only | adopt as optional feature |
| `pulldown-cmark` 0.13 | safe Markdown-to-terminal-span parsing | adopt |
| `regex` 1.13 | bounded linear-time transforms/rules | adopt |
| `globset` 0.4 | command/cwd/config selectors | adopt |
| `rustix` 1.1 | Unix peer credentials, process groups, and low-level safety | adopt narrowly |
| `jsonc-parser` 0.33 | VS Code JSONC snippet files | adopt |
| `serde_json` 1.0 | schemas, snippets, adapter manifests | adopt |
| `winnow` 1.0 | LSP/TextMate snippet grammar | adopt |
| `blake3` 1.8 | context/cache/content fingerprints | adopt |
| `semver` 1.0 | protocol and adapter version constraints | adopt |
| `indexmap` 2.14 | deterministic configured source/adapter ordering | adopt |
| `bitflags` 2.13 | item and adapter capabilities | adopt |
| `tracing` 0.1 | structured observability | adopt |
| `tracing-subscriber` 0.3 | file/filter logging | adopt |
| `thiserror` 2.0 | typed library errors | adopt |
| `anyhow` 1.0 | CLI/application error context | adopt outside core model |
| `clap` 4.6 | `zsh-sense` installer/doctor/config CLI | adopt |
| `ureq` 3.3 | blocking HTTPS in explicit install/update commands | adopt in CLI only, with Rustls |
| `minisign-verify` 0.2 | verify prebuilt adapter/release signatures | adopt in CLI only |
| `tar` 0.4 and `zstd` 0.13 | unpack bounded release artifacts | adopt narrowly in CLI |
| `tempfile` 3.27 | atomic staged downloads/builds/self-tests | adopt |
| `bindgen` 0.72 and `cc` | generate/compile exact Zsh bindings and any macro shim | build dependencies only |

### 12.2 Adapter-specific candidates

| Library | Adapter/use | Decision |
|---|---|---|
| `gix-discover` 0.54 | fast Git repository discovery | adopt for Git adapter |
| `gix` 0.86 | in-process Git metadata | benchmark against supervised `git`; do not adopt blindly |
| `zbus` 5.18 | systemd D-Bus data | adopt for Linux systemd adapter |
| `cargo_metadata` 0.23 | Cargo workspace/target data | adopt |
| `bollard` 0.21 | Docker/Podman API | optional adapter dependency |
| `sysinfo` 0.39 | process metadata | optional process adapter dependency |
| `lsp-types` 0.97 / `async-lsp` 0.2 | optional LSP conversion/bridge | defer until the bridge milestone; never expose these types inside the core model |

For Git operations, the initial implementation may use `gix-discover` plus a
supervised `git` process. This honors user Git configuration and avoids pulling
the full `gix` feature graph before benchmarks show a benefit.

The Carapace importer must prefer an upstream compiled JSON representation if
one is available. We will not add deprecated `serde_yaml`; if direct YAML
ingestion is still necessary at that milestone, the then-current permissively
licensed YAML crates will receive a separate parser/fuzz audit before one is
selected.

### 12.3 Test and quality tooling

| Library/tool | Use |
|---|---|
| `criterion` | latency and throughput benchmarks |
| `proptest` | ranges, ranking, parser, merge, and config properties |
| `insta` | popup/layout and diagnostic snapshots |
| `expectrl` | PTY/ZLE end-to-end interaction |
| `tempfile` | isolated test homes, sockets, projects, and databases |
| `cargo-fuzz`/libFuzzer | protocol, snippet, schema, and syntax fuzz targets |
| Miri | unsafe wrapper and pure-Rust invariant checks where applicable |
| sanitizers | native adapter and Frizbee integration checks |
| `cargo-deny`, `cargo-audit`, `cargo-vet` | license, advisory, and supply-chain policy |

### 12.4 Deliberately rejected or deferred

| Library/approach | Reason |
|---|---|
| published `zsh-sys` as direct foundation | too broad/raw; internal allowlisted sys crate gives control |
| full LSP/JSON-RPC for the hot protocol | document/URI mismatch, JSON volume, no multi-source completion streaming |
| `lsp-types` as the internal model | loses Zsh/raw-byte/group/source semantics; conversions are cleaner |
| `ratatui`/`crossterm` | would compete with ZLE for terminal ownership |
| `fzf` in continuous completion | transfers input and creates an unnecessary interactive process |
| `editor-core` for snippets | complete editor engine is far beyond the needed parser/session logic |
| GPL-only snippet engine | avoid forcing an accidental project license decision |
| `ropey`/piece-table editor core | command buffers are short and ZLE already owns editing |
| `sqlx` | unnecessary async/database scope for one small local SQLite actor |
| arbitrary `--help` execution | startup code may be slow or have side effects; require trust policy |
| in-process third-party adapter dylibs | one faulty adapter could corrupt the daemon |

## 13. Feature scope

The checkboxes below are implementation tracking, not optional brainstorming.

### 13.1 Editing and activation

- [x] Continuous completion is the default.
- [ ] Manual, hybrid, and disabled activation modes.
- [x] Tab/manual trigger regardless of automatic mode.
- [ ] Configurable behavior for insert, delete, backspace, word-delete, paste,
      history, cursor, and accept events.
- [ ] Immediate structural triggers for paths, options, assignments, and
      subcommand boundaries.
- [ ] Adaptive/user-configured debounce.
- [x] Cancellation and stale-generation rejection.
- [ ] Manual dismiss and context suppression.
- [x] Non-modal typing while the popup is present.
- [x] Post-accept chaining for directories, options, and nested contexts.

### 13.2 Candidate generation and correctness

- [x] Universal `_main_complete` source.
- [ ] Native and portable capture backends.
- [ ] Real `compadd` candidate capture in native mode.
- [x] Zsh descriptions, explanations, groups, prefixes, suffixes, and relevant
      metadata.
- [x] Correct acceptance through Zsh match selection.
- [ ] Explicit text-edit and snippet insertion strategies.
- [ ] Byte-safe non-UTF-8 path/argument handling.
- [x] Stable item identity and semantic deduplication.
- [x] Candidate kinds, tags, source provenance, and confidence.

### 13.3 Matching and ranking

- [x] Frizbee fuzzy subsequence matching.
- [x] Prefix-bounded short-query generation before fuzzy broadening.
- [x] Adaptive typo tolerance.
- [x] Exact, prefix, boundary, context, and original-order ranking.
- [x] Group-preserving ranking.
- [ ] Calibrated cross-source relevance rather than raw score mixing.
- [ ] Incremental cached refiltering.
- [ ] Project-aware proximity and optional frecency.
- [ ] Positive successful-use and bounded negative feedback.
- [ ] Ranking inspector/explain mode.

### 13.4 Popup and navigation

- [x] VS Code-inspired bordered panel rendered by ZLE.
- [ ] Full, minimal, and undecorated popup styles.
- [ ] Responsive list-only, side-documentation, and below-documentation modes.
- [ ] Selected row and matched-character highlighting.
- [ ] Groups, descriptions, item kinds, optional icons, and ASCII fallbacks.
- [ ] Correct Unicode/grapheme/cell-width layout.
- [ ] Configurable rows, width, padding, title, footer, count, and scrollbar.
- [ ] Item navigation, page navigation, and independent documentation scroll.
- [ ] Loading, timeout, partial-result, and source-error states.
- [ ] Immediate terminal-resize relayout.

### 13.5 Documentation and signatures

- [x] Cheap Zsh completion descriptions.
- [ ] Lazy selected-item documentation resolution.
- [ ] Schema and man-page documentation.
- [ ] Trusted, supervised help-output extraction.
- [ ] Markdown parsing into safe terminal spans.
- [ ] Documentation source attribution and cache invalidation.
- [ ] Signature help and current argument highlighting.
- [ ] Expected-value/type hints.

### 13.6 Parsing and schemas

- [ ] Incremental `tree-sitter-zsh` parsing.
- [ ] Pipelines, substitutions, wrappers, assignments, redirections, and
      quoting context.
- [ ] Reconciliation with real Zsh `words`, `CURRENT`, `PREFIX`, and `SUFFIX`.
- [ ] Context epochs for cwd, environment, aliases, functions, compdefs,
      styles, project roots, and executable changes.
- [ ] Normalized command/subcommand/option/argument schema IR.
- [ ] Declarative Fig/Carapace importers compiled ahead of the hot path.
- [ ] First-party and user schema overrides.
- [ ] Required/repeatable/conflicting/dependent argument constraints.
- [ ] Explicit schema authority/confidence levels.
- [ ] Nested command discovery and optional hierarchy navigation.

### 13.7 Context adapters

- [ ] Built-in and external adapter API from the first architecture release.
- [ ] Completion, enrichment, resolve, signature, diagnostic, action, and
      preview capabilities.
- [ ] Adapter manifests, config schemas, trust declarations, deadlines,
      cancellation, and resource limits.
- [ ] Out-of-process third-party adapter supervision.
- [ ] Git adapter and lazy previews.
- [ ] systemd adapter and lazy status.
- [ ] Cargo/npm project metadata adapters.
- [ ] Optional Docker/Podman and process adapters.
- [ ] Latency-budget scheduler that keeps adapters off the keypress path.

### 13.8 Ghost text and history

- [ ] End-of-buffer ghost text.
- [ ] Token/word/path-segment partial acceptance.
- [ ] History and completion-derived continuations.
- [ ] Confidence suppression for ambiguous suggestions.
- [ ] Structured history segmentation rather than blind whole-line insertion.
- [ ] Local contextual frecency database.
- [ ] Successful-execution learning, sensitive filtering, and incognito mode.
- [ ] User inspection, export, reset, and disable controls.

### 13.9 Snippets

- [ ] VS Code JSONC snippet-file import.
- [ ] LSP/TextMate tabstops, placeholders, final cursor, choices, variables,
      mirrors, escaping, and transforms.
- [ ] Snippet candidates with configurable ranking.
- [ ] Next/previous placeholder navigation.
- [ ] Linked placeholder editing and choice popup.
- [ ] Multiline snippets, correct range updates, cancellation, and undo tests.
- [ ] User-global and explicitly trusted project snippets.

### 13.10 Diagnostics, actions, and safety

- [ ] Conservative incomplete-syntax hints.
- [ ] Authoritative unknown-option, invalid-value, missing-value, conflict, and
      type diagnostics.
- [ ] Buffer underlines and popup diagnostic details.
- [ ] Deterministic spelling fixes and code actions.
- [ ] Previewable, undo-safe edits.
- [ ] Confidence/severity visibility.
- [ ] Deterministic destructive-command safety rules.
- [ ] Factual target/device context and safer-alternative actions.
- [ ] Optional narrowly scoped execution confirmation.
- [ ] No AI or fuzzy rule may block command execution.

### 13.11 Zsh-language intelligence

- [ ] Parameters, arrays, associative arrays, functions, aliases, and builtins.
- [ ] Parameter expansion flags and explanations.
- [ ] Glob qualifiers and explanations.
- [ ] Redirection/file-descriptor hints.
- [ ] Arithmetic and shell construct context where parser confidence permits.

### 13.12 Configuration, diagnostics, and lifecycle

- [ ] Complete TOML configuration surface and generated schema.
- [ ] Live atomic reload and deterministic rules.
- [x] State-specific, configurable emacs/vi keybindings.
- [ ] Icon/text/no-indicator and documentation/popup decoration controls.
- [ ] Per-source, per-adapter, per-command, cwd, environment, and profile
      controls.
- [ ] `config`, `doctor`, `adapter`, `cache`, `history`, and `explain` CLI
      commands.
- [x] Structured logs that redact command lines by default.
- [ ] Daemon autostart, crash isolation, reconnect, and recovery.
- [ ] Bounded protocol, memory, caches, output, and backpressure.
- [ ] Signed/checksummed native adapter installation and exact ABI selection.
- [ ] Portable fallback and normal-ZLE graceful degradation.

## 14. Implementation milestones

Every milestone builds final components. There is no C bridge or throwaway
runtime.

### M0 — Freeze the contracts

- finalize this plan through review;
- define model, protocol, config, adapter, and ABI versioning policies;
- create behavioral fixtures from product requirements rather than C output;
- record baseline Zsh completion latency/candidate-size measurements;
- audit and pin the initial dependency graph.

Exit: approved design, config schema draft, protocol threat model, benchmark
corpus, and no unresolved foundational process boundary.

### M1 — Rust workspace and native portability foundation

- create the Cargo workspace and final crate boundaries;
- implement `sense-zsh-sys` allowlisted generation;
- implement adapter probing, ABI keys, installer/cache layout, and isolated
  load self-test;
- implement the greenfield native adapter capture skeleton and portable
  backend skeleton;
- establish Zsh release/patchlevel CI fixtures.

Exit: current Zsh and at least one different supported release can each use
their own automatically selected module, and portable mode works without one.

### M2 — Model, protocol, daemon, and adapter API

- implement the LSP-inspired core models and raw-byte rules;
- implement bounded MessagePack framing, sessions, generations, cancellation,
  streaming, and backpressure;
- implement daemon lifecycle, Unix socket security, config loading/reload,
  source API, context adapter API, and supervision;
- implement CLI `init`, `install`, `doctor`, and `config` foundations.

Exit: multiple synthetic shell sessions can stream deterministic candidate
batches through the final protocol with cancellation and config reload.

### M3 — Zsh source, parsing, caching, and ranking

- complete native/portable Zsh capture and Zsh acceptance;
- integrate tree-sitter context reconciliation;
- integrate Frizbee, groups, stable identity, deduplication, and match spans;
- implement Moka caches, context epochs, incremental refiltering, and latency
  scheduling;
- implement filesystem and history/frecency sources.

Exit: all core examples complete correctly, including non-UTF-8 fixtures,
quotes, paths, options, backspace, and structural retriggers.

### M4 — Continuous ZLE UX

- implement configurable continuous/manual/hybrid activation;
- implement state-specific keybindings and original-widget delegation;
- implement responsive popup, style spans, navigation, resize, loading/error
  states, and stable selection;
- implement descriptions, groups, kinds, icon modes, and matched highlighting;
- implement ghost text and partial acceptance.

Exit: continuous completion is the stable default and no provider can block
editing.

### M5 — Documentation, schemas, and snippets

- implement lazy documentation and signature models/UI;
- implement man/schema/trusted-help resolvers;
- implement schema IR and declarative import pipeline;
- implement `sense-snippet`, VS Code JSONC import, snippet sessions, choices,
  mirrors, and transforms;
- complete config/schema documentation.

Exit: flags show descriptions/docs/signatures, schemas provide typed argument
context, and snippets pass multiline/mirror/undo PTY tests.

### M6 — Context adapters

- implement Git and systemd adapters first;
- implement Cargo/npm declarative project metadata;
- finalize external adapter manifest, SDK, supervision, and trust UX;
- add Docker/Podman and process adapters behind optional features/config;
- implement lazy previews and adapter-specific cache invalidation.

Exit: adapter failures/timeouts do not delay the base popup, and dynamic
metadata visibly enriches existing Zsh candidates.

### M7 — Diagnostics, actions, safety, and Zsh language intelligence

- implement confidence-aware schema/syntax diagnostics;
- implement deterministic actions and previewable edits;
- implement parameter flags, glob qualifiers, redirection and core Zsh
  language intelligence;
- implement factual safety rules and optional confirmations.

Exit: diagnostics never claim certainty from partial metadata, actions are
undo-safe, and safety behavior is deterministic and configurable.

### M8 — Hardening and release engineering

- fuzz protocol, snippet grammar, schema import, syntax projection, and edit
  application;
- run Miri/sanitizers on applicable boundaries;
- add Linux/macOS and Zsh version/patchlevel CI matrix;
- test daemon/adapters killed at every request phase;
- enforce dependency licenses/advisories and reproducible source checksums;
- package release assets and package-manager integration;
- freeze protocol/config v1 compatibility rules.

Exit: performance budgets and reliability gates pass on the release matrix,
installation handles Zsh version differences automatically, and failure never
breaks ordinary command-line editing.

## 15. Performance budgets

Initial budgets, to be revised only with measurements:

```text
ZLE edit/enqueue overhead                    p95 < 1 ms
cached refilter + rank + layout round trip   p95 < 8 ms
first cheap-source update                    p95 < 20 ms after dispatch
default debounce                             35 ms
popup redraw preparation                     p95 < 2 ms
Zsh source soft deadline                     100 ms
Zsh source default hard deadline             1000 ms
selected-item cheap resolve                  p95 < 50 ms
```

Additional rules:

- provider and adapter work never blocks ZLE;
- stale results are discarded before layout/render;
- cache and batch sizes are bounded by configured byte weights, not only item
  counts;
- Frizbee starts with `safe_read`; disabling it requires measured necessity
  and dedicated sanitizer/platform gates;
- parallel fuzzy matching is used only above a measured candidate threshold;
- expensive documentation and previews are selection-lazy;
- external processes run in cancellable process groups with output caps.

## 16. Security and privacy

- Runtime socket lives under `$XDG_RUNTIME_DIR`, is user-only, and validates
  peer credentials where supported.
- Every protocol collection and frame has a hard maximum.
- No raw ANSI/control sequences from candidates, docs, or adapters are emitted
  to the terminal.
- External adapter and command output is treated as untrusted data.
- Arbitrary command `--help` is never run without an explicit trust rule.
- Project-local schemas, snippets, config, and adapters require trust.
- History/frecency stays local, uses private permissions, and supports
  sensitive filters/incognito/reset.
- Logs exclude command lines and environment values by default.
- Adapter downloads require checksums/signatures and an explicit policy.
- Third-party adapters are processes, not in-process libraries.
- The native module contains the minimum unsafe surface and no network or
  provider code.

## 17. Testing strategy

### 17.1 Unit/property tests

- item merge/deduplication, ranking, stable selection, context epochs;
- byte/UTF-8/grapheme/cell coordinate conversions;
- config defaults, merging, validation, and live replacement;
- snippet grammar, transformations, mirror range updates;
- protocol bounds and cancellation state machines;
- schema authority and diagnostic confidence.

### 17.2 Differential and fixture tests

- incomplete and complete Zsh syntax corpus;
- parser projection compared with live Zsh completion context;
- official Zsh releases and development patchlevels;
- quoting, metacharacters, glob qualifiers, substitutions, pipelines, and
  redirects;
- filenames containing spaces, newlines, control escapes, and invalid UTF-8.

### 17.3 PTY tests

- emacs and vi insert keymaps;
- continuous and manual/Tab activation;
- insert, backspace, delete, paste, history, movement, and resize;
- navigation, pages, docs scrolling, ghost text, and snippets;
- correct acceptance and undo behavior;
- daemon/module/adapter crashes and reconnect;
- absence of literal ANSI escape sequences.

### 17.4 Performance tests

- representative `cd`, `ls -`, `git`, `systemctl`, package, and project
  contexts;
- 10, 1,000, 100,000, and million-item synthetic sets;
- cached versus regenerated queries;
- ASCII, Unicode, typo, and path workloads;
- warm and cold daemon/database/documentation caches.

## 18. Planned workspace

```text
zsh-sense/
├── Cargo.toml
├── Cargo.lock
├── PLAN.md
├── crates/
│   ├── sense-model/
│   ├── sense-protocol/
│   ├── sense-config/
│   ├── sense-daemon/
│   ├── sense-cli/
│   ├── sense-zsh-abi/
│   ├── sense-zsh-sys/
│   ├── sense-zsh-module/
│   ├── sense-zsh-worker/
│   ├── sense-rank/
│   ├── sense-syntax/
│   ├── sense-layout/
│   ├── sense-snippet/
│   ├── sense-provider-api/
│   ├── sense-providers/
│   ├── sense-adapter-api/
│   └── sense-adapters/
├── shell/
│   └── zsh-sense.plugin.zsh
├── schemas/
├── snippets/
├── adapters/
├── tests/
│   ├── fixtures/
│   ├── fuzz/
│   └── pty/
└── xtask/
```

There is no legacy C subtree in the intended final workspace.

## 19. Explicit non-goals for the core

- A standalone terminal application that owns keyboard input.
- A true GUI overlapping window on terminals that do not provide one.
- Mid-buffer ghost text implemented by temporarily corrupting/mutating BUFFER.
- Replacing Zsh's completion definitions with handwritten schemas.
- Handwritten context adapters for every command.
- Running arbitrary generator/help code on each keystroke.
- Using an AI model for baseline completion, validation, ranking, or execution
  blocking.
- Supporting Bash/Fish in protocol v1; the models should not prevent future
  clients, but Zsh correctness comes first.

## 20. Primary references

- [Zsh Line Editor](https://zsh.sourceforge.io/Doc/Release/Zsh-Line-Editor.html)
- [Zsh Completion Widgets](https://zsh.sourceforge.io/Doc/Release/Completion-Widgets.html)
- [Zsh Completion System](https://zsh.sourceforge.io/Doc/Release/Completion-System.html)
- [Zsh Modules](https://zsh.sourceforge.io/Doc/Release/Zsh-Modules.html)
- [Zsh parameters: ZSH_VERSION and ZSH_PATCHLEVEL](https://zsh.sourceforge.io/Doc/Release/Parameters.html)
- [Language Server Protocol 3.18](https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.18/specification.md)
- [VS Code snippets](https://code.visualstudio.com/docs/editing/userdefinedsnippets)
- [Frizbee](https://github.com/saghen/frizbee)
- [Blink fuzzy Rust integration](https://github.com/Saghen/blink.cmp/blob/main/lua/blink/cmp/fuzzy/rust/fuzzy.rs)
- [zsh-sys reference implementation](https://github.com/zsh-rs/zsh-sys)
- [tree-sitter-zsh](https://github.com/georgeharker/tree-sitter-zsh)
- [devicons](https://docs.rs/devicons/latest/devicons/)
- [Fig completion specifications](https://github.com/withfig/autocomplete)
- [Carapace specifications](https://github.com/carapace-sh/carapace-spec)
