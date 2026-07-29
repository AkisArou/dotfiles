# Shell Sense implementation plan

Status: active implementation  
Last updated: 2026-07-29

Current format boundaries: configuration v4, semantic model v4, client protocol
v7.0, and provider API v4. Older shapes are rejected rather than migrated
in-process.

## 1. Product

Shell Sense is a continuous, editor-style completion and documentation client
for interactive shells. It gives Zsh, Fish, and Bash one product experience
without replacing the completion definitions users already have.

The active shell is the sole authority for which candidates are valid:

```text
Zsh session  -> Zsh completion system (`_main_complete`, compdefs)
Fish session -> Fish completion system (`complete -C`, user completions)
Bash session -> Bash programmable completion (compspecs, bash-completion)
```

Shell Sense owns invocation policy, fuzzy ranking, presentation, selection,
documentation, cancellation, and caching. It never fills gaps with its own
filesystem, PATH, history, command-spec, or Carapace candidate source.

The current scope is completion, IntelliSense-style presentation, and
documentation. Snippets, diagnostics, command rewriting, AI, and safety
interposition are intentionally outside this plan.

## 2. Non-negotiable rules

1. Exactly one native candidate authority exists per interactive session.
2. Context adapters may enrich a native item; they cannot create candidates.
3. Successful candidate acceptance is performed and explicitly acknowledged
   by the shell that produced the item. A native worker may report only a
   pre-edit rejection when its generation-bound acceptance route no longer
   exists.
4. Shell line editors retain ownership of input, the editable buffer, cursor,
   history, keymaps, and redraw.
5. Rust never reads commands from the terminal and never executes a selected
   command.
6. Continuous completion is the default. Manual-only activation and Tab as a
   trigger remain first-class configuration.
7. Exact and broadened requests go to the same native provider. Broadening
   removes a fuzzy fragment while retaining structure such as `--`, `$`, an
   option-value `=`, or a path prefix. It does not manufacture candidates.
8. Display text is never reused as insertion text. Raw insertion data and an
   opaque, request-scoped acceptance identity are retained separately.
9. Every asynchronous result carries a session, request, and generation.
   Stale generations can neither render nor be accepted.
10. No compatibility aliases or legacy protocol/config shapes remain after a
    refactor. The protocol major changes when a wire shape changes.
11. Shell Sense has one semantic model and client protocol. Shell-specific
    behavior is isolated behind native-provider and line-editor boundaries.
12. The hot path contains no `fzf`, Carapace, Node, Python, or subprocess per
    keystroke.

## 3. Architecture

```text
                         active interactive shell
                    buffer / cursor / native completion
                                  |
                                  v
                  +-------------------------------+
                  | shell integration             |
                  | edits, trigger, native capture|
                  | render, navigate, accept      |
                  +---------------+---------------+
                                  |
                  private framed session transport
                                  |
                                  v
                  +-------------------------------+
                  | per-shell Rust worker         |
                  | normalize, bound, retain      |
                  | acceptance routing, viewport  |
                  +---------------+---------------+
                                  |
                          MessagePack / Unix socket
                                  |
                                  v
                  +-------------------------------+
                  | persistent Rust daemon        |
                  | sessions, cancellation        |
                  | Frizbee ranking, docs, adapters|
                  +---------------+---------------+
                                  |
                       enrichment / documentation
                                  |
                                  v
                  +-------------------------------+
                  | context adapters              |
                  | never candidate producers     |
                  +-------------------------------+
```

The daemon is one process per user. A worker is one process per interactive
shell session because native completion depends on live aliases, functions,
variables, options, jobs, and user completion registrations.

Only the active shell provider runs. A Zsh session never starts Fish or Bash
to collect more candidates.

An editor presenter is a second client of that same live session, not a second
completion provider. When attached, it receives daemon-ranked views while the
shell continues to own generation and acceptance.

## 4. Shared model

The shared completion item is intentionally close to LSP's `CompletionItem`
where the semantics overlap:

```rust
CompletionItem {
    id,
    source,               // exactly zsh, fish, or bash
    label,
    label_detail,
    filter_text,
    kind,
    detail,
    documentation,
    group,
    edit,
    insertion: NativeMatch { shell, fingerprint },
    original_order,
    confidence,
    capabilities,
    match_result,
    resource,             // typed native resource, currently filesystem path
    opaque_data,
}
```

We do not use LSP JSON-RPC on the latency-sensitive wire. LSP assumes editor
documents, UTF-16 positions, and text edits that the client can apply itself.
Shell Sense needs arbitrary Unix bytes, shell-owned insertion, native match
identity, streaming generations, and terminal layout metadata. Conversion to
an LSP-like API for a Blink source is kept at the boundary.

## 5. Native provider contract

`NativeCompletionProvider` is the only API capable of returning candidates.
It declares:

- its `NativeShell`;
- description, group, kind, broad-query, documentation, and partial-accept
  capabilities;
- hard candidate bounds;
- soft and hard deadlines;
- cancellation support.

`NativeCandidate` has no arbitrary source identifier. Normalization assigns
the source from `NativeShell` and creates a shell-discriminated
`NativeMatch`. The daemon validates the same invariant at the trust boundary.

A completion request has two possible query modes:

- `Exact`: unchanged shell buffer and cursor;
- `Broad`: the fuzzy fragment is removed and the native provider is asked for
  the wider, still context-valid universe.

Examples:

```text
systemctl rstart -> systemctl <native completion> -> rank `restart`
ls --recusr      -> ls --<native completion>      -> rank `--recursive`
cd dotfiles/nv   -> cd dotfiles/<native completion> -> rank `nvim/`
cmd --color=au   -> cmd --color=<native completion> -> rank `auto`
```

The original buffer remains the ranking query and acceptance target.

## 6. Shell adapters

### 6.1 Zsh

Zsh is the behavior reference and currently works end to end.

- `_main_complete` dispatches all registered completion functions.
- The live Zsh provider captures `compadd` metadata in live ZLE.
- Groups, display strings, descriptions, prefixes, suffixes, path behavior,
  and opaque match identity are retained.
- Acceptance is replayed through Zsh completion state, never pasted.
- `POSTDISPLAY` and `region_highlight` provide stable, non-scrollback UI.

Zsh exposes no supported hook around `compadd` or its internal `addmatches`
operation. Intercepting an explicit `builtin compadd` call from a module would
require replacing the handler in Zsh's global builtin hash table. Shell Sense
rejects that undocumented mutation: the empty native-module scaffold and its
ABI fields were removed rather than retained as dormant release code. Native
interception will be reconsidered only if Zsh exposes a supported hook (or a
Shell Sense contribution adds one upstream).

### 6.2 Fish

Fish has the cleanest cross-shell native interface:

- call `complete -C STRING --escape` inside the live Fish session;
- split the native candidate and optional tab-separated description;
- preserve Fish's escaped insertion separately from display text;
- infer item kind only from native metadata such as `directory` or `command`;
- apply acceptance through `commandline` in the owning Fish process;
- use the generic binding (`bind ''`) to schedule continuous completion after
  unmatched self-insert keys, plus explicit wrappers for destructive edits;
- keep Tab/manual triggering configurable.

No separate Fish process is used for candidate generation because it would
not share the exact live function/variable/completion state.

### 6.3 Bash

Bash uses its live programmable-completion registry:

- locate the active compspec with `complete -p`;
- support functions (`-F`), commands (`-C`), word lists, glob/actions,
  filters, prefixes/suffixes, and completion options;
- invoke bash-completion's lazy loader when installed;
- populate `COMP_LINE`, `COMP_POINT`, `COMP_WORDS`, `COMP_CWORD`, `COMP_TYPE`,
  and `COMP_KEY` for the request;
- collect only `COMPREPLY` and native default/bashdefault results;
- preserve `nospace`, `filenames`, `dirnames`, and quoting behavior for
  acceptance.

Bash exposes no public equivalent of Zsh `_main_complete` or Fish
`complete -C` for an arbitrary line. `COMP_WORDS` normally exists only while
Readline invokes completion. Shell Sense therefore isolates and tests a small
context-reconstruction layer for continuous requests. This is an input
adapter, not a candidate source; registered Bash compspecs remain the sole
candidate authority.

Manual Tab completion can use the exact Readline context and is the strongest
fidelity path. Continuous Bash support must never be advertised as identical
to Zsh/Fish where Bash's public API cannot provide that guarantee.

Bash also lacks a safe public callback for applying delayed documentation while
Readline is idle. Its signal handler may interrupt Readline, but it must not run
the array-heavy mailbox decoder on Readline's signal stack. Bash therefore
consumes delayed documentation on the next Shell Sense action. ZLE and Fish can
apply the same update immediately from their supported editor callbacks.

## 7. Context adapters and documentation

`ContextAdapter` supports only:

- `enrich`: add kind, detail, tags, or already-known documentation to native
  items;
- `resolve`: lazily resolve documentation for a selected native item.

An enrichment references an existing `ItemId`. Unknown identifiers are
discarded. Adapters cannot emit a candidate event, and the daemon rejects a
candidate batch from an adapter role.

The active shell publishes its own token vector and current-word index as a
separate, generation-bound context event. The daemon retains that context and
broadcasts it to attached adapters. It does not apply a generic shell parser
to reconstruct Bash, Fish, or Zsh syntax.

Documentation precedence is deterministic:

1. a command-specific context adapter for an existing native item, including
   focused help and factual runtime/project details;
2. the first matching configured documentation resolver;
3. local man pages for native options when neither higher-priority adapter
   matches.

Native completion descriptions/explanations remain menu detail and are not
duplicated into the documentation pane. Filesystem candidates carry a typed,
byte-preserving resource path supplied by the native shell adapter. Configured
resolvers consume a single `$value` placeholder as a complete argv entry. It
is the native typed resource for filesystem items and the semantic label for
other kinds. Shell Sense never evaluates a command string or reconstructs a
path from UI text. Resolver executables are looked up directly through the
daemon environment's `PATH`; interactive-shell aliases and functions are not
consulted. Resolver output is normalized to plain text before presentation.

Generic option documentation uses the local man page only as a fallback when
no command-specific adapter matches. Manual output is de-overstruck in Rust,
focused on the selected native option, and cached by command context rather
than by the transient fuzzy fragment. Dynamic Git refs and systemd unit state
are deliberately excluded from the long-lived documentation cache.

Initial context adapters are Git and systemd because they can add high-value
command/service kinds and selected-item documentation without changing
validity. Their enrichment path performs no I/O. Bounded, cancellable `git`
or `systemctl` inspection runs only after the selected item's documentation
delay.

## 8. Matching and ordering

Frizbee is the textual matcher. Shell Sense does not implement a second fuzzy
algorithm.

Ranking combines:

- exact and case-sensitive prefix matches;
- Frizbee fuzzy score and match spans;
- word/path/kebab/underscore boundaries;
- native group and original order;
- native metadata confidence;
- stable selection across refreshed views.

Ranking never moves an item across a semantically distinct native group unless
the shell did not provide groups. Matching spans are sent to both terminal and
Blink clients for highlighted labels.

Local refiltering is allowed only over the current native candidate set. It
must be invalidated when command structure, cwd, environment epoch, or native
context changes.

## 9. UI and clients

The terminal UI provides:

- a responsive-width completion menu;
- selected-row highlighting;
- fuzzy match highlighting;
- kind icons, text labels, or no indicators;
- dimmed kind/detail/source columns;
- optional descriptions and group headings;
- a correctly proportional scrollbar;
- optional border/decorations/title/footer;
- a side or below documentation pane;
- completion-derived ghost text;
- configurable rows, widths, colors, debounce, triggers, and keybindings.

Default behavior:

```text
activation      continuous
Tab closed      trigger
Tab open        accept
Ctrl-N          next
Ctrl-P          previous
Ctrl-D          page down
Ctrl-U          page up
Ctrl-E          accept
Escape          close
Ctrl-C          interrupt and erase UI
```

Zsh renders through ZLE. Fish renders through Fish's line-editor callbacks and
repaint operation. Bash renders through Readline bind/trap integration and
redisplay. The daemon sends semantic items/layout; it does not own terminal
input.

The Blink client attaches to the live session by the owning terminal shell
PID. The first external presenter suppresses the terminal popup without
discarding shell state; the last disconnect restores terminal presentation.
It maps the same model to Blink completion items, preserving label, kind,
detail, documentation, byte edit range, match spans, and resolve identity.
Blink may filter and render those items, but accepting one sends its opaque ID
back through the daemon and waits for ZLE/Fish/Readline to report whether the
native edit was applied. Neovim never pastes the presentation edit itself.

Blink exposes only the current terminal row. The source translates full
shell-buffer byte ranges onto the last row and omits an edit that cannot be
represented safely. OSC 133 command-start marks provide an exact prompt
boundary; exact suffix matching against the native request is the fallback.

## 10. Configuration

The application namespace is `shell-sense`. Operational environment variables
use `SHELL_SENSE_*`; typed configuration overrides use the disjoint
`SHELL_SENSE_CONFIG__*` namespace with `__` between nested keys.

Configurable areas include:

- activation mode, debounce, trigger characters, after-accept behavior;
- shell-specific broad-query threshold and candidate bounds;
- closed/open popup keymaps;
- kind indicators: icons, text, or none;
- popup size, padding, border, decorations, scrollbar, descriptions, groups;
- all UI styles and per-kind styles;
- documentation mode, delay, size, and Markdown rendering;
- ghost text and partial acceptance;
- matcher result limits and typo tolerance;
- adapter deadlines, concurrency, Git/man/systemd enablement, and ordered
  typed documentation resolver commands;
- documentation cache bounds and logging level.

Unknown fields are errors. Named profiles layer before the main config. A JSON
Schema and `config check/effective/schema/paths` commands remain part of the
CLI.

## 11. Transport, cancellation, and security

- Daemon transport: private mode-0600 Unix socket, length-prefixed MessagePack.
- Zsh worker transport: immediately unlinked private FIFOs and binary-safe
  netstrings.
- Fish/Bash worker transport: private mode-0600 input FIFOs and acknowledged,
  reusable output mailboxes with signal notification and the same bounded
  netstring messages.
- All frame, field, item count, candidate byte, and adapter output sizes are
  bounded.
- Only a shell client can create a session.
- Presentation clients attach to an existing session by its owning shell PID;
  they cannot create sessions or publish candidates.
- A completion worker must declare the same native shell as its session.
- Only the completion-worker role can publish candidates.
- Only the owning shell can report successful native application; a completion
  worker can report only a pre-application routing rejection.
- Every item source and `NativeMatch.shell` must equal the session shell.
- Control characters in display fields are escaped before rendering.
- Command buffers are excluded from logs.
- Daemon discovery requires a current-protocol handshake, not merely a live
  socket. Replacement requires an explicit incompatible-protocol response and
  same-user peer credentials.

## 12. Performance budgets

Measured in release builds after warm-up:

```text
ordinary edit overhead while no request runs     p95 < 1 ms
cached native-set refilter                       p95 < 5 ms
daemon rank/layout for 10,000 candidates          p95 < 8 ms
first visible update for cheap native completion  p95 < 30 ms
popup navigation and acceptance                  synchronous, < 1 ms
```

Policies:

- debounce automatic edits, never manual triggers;
- cancel superseded generations immediately;
- do not start expensive completion while shell input is already queued;
- keep one look-ahead viewport in the shell worker;
- stream bounded batches and never serialize the full set to the renderer;
- cache only stable documentation with explicit context epochs;
- retain one active acceptance generation per session.

## 13. Dependencies

Chosen libraries and responsibilities:

- `frizbee`: fuzzy scoring and match positions;
- `tokio` / `tokio-util`: daemon, cancellation, sockets, framing, signals;
- `serde` / `rmp-serde`: typed internal wire format;
- `clap`: CLI;
- `figment` / `schemars`: strict layered config and JSON Schema;
- `proptest`: fragmentation and hostile-input properties for both bounded wire
  codecs;
- `criterion`: reproducible release-mode latency benchmarks;
- `unicode-width`: terminal cell layout;
- `blake3`: opaque request identities and cache keys;
- `moka`: bounded, weighted, expiring documentation cache;
- `nix`: safe process signal and identity APIs for mailbox transport and
  incompatible-daemon replacement;
- `tempfile`: same-filesystem installation transactions and atomic executable
  publication;
- `devicons`: evaluate for file icons only after native file-kind fidelity is
  established; it never participates in candidate generation;

Carapace, fzf, LSP servers, and snippet crates are not dependencies of the
native-only product scope.

## 14. Delivery phases

### Phase A — Native authority foundation (implemented)

- `NativeShell` and shell-discriminated `NativeMatch`;
- protocol v7 shell identity, native-only insertion semantics, typed resources,
  native context,
  and daemon-enforced adapter events;
- session bound to one native source;
- cross-shell worker handshake;
- adapter candidate publication rejected;
- native-provider/context-adapter APIs separated;
- exact/broad query model and structural broadening;
- Zsh normalization migrated to the shared native candidate path;
- Fish/Bash generic capture store and acceptance routes;
- strict Fish end-to-end daemon/worker/rank/select test;
- product/crate/config namespace renamed to Shell Sense;
- signal-driven Fish/Bash mailbox transport.

### Phase B — Zsh reference parity (implemented, regression gate)

- continuous/manual invocation;
- fuzzy matching, paths, flags, descriptions, groups;
- stable dynamic popup, navigation, acceptance, cancellation;
- ghost text and partial acceptance;
- interactive PTY regression tests and real dotfiles smoke test.

### Phase C — Fish native integration (implemented)

- live `complete -C` exact and broadened capture;
- raw/display/description separation and kind normalization;
- generic edit/deletion bindings with configurable manual mode;
- mailbox event integration;
- Fish-owned selection and path chaining;
- same semantic popup and documentation protocol;
- interactive Fish PTY tests, including custom user completions.

### Phase D — Bash native integration (implemented)

- live compspec interpreter using the active shell's registered completions;
- tested context reconstruction for automatic requests;
- native defaults/options/quoting capture;
- Readline keymaps, mailbox events, rendering, and acceptance;
- manual Tab exact-context path;
- interactive Bash PTY tests with custom and bash-completion providers.

The provider fixtures cover function and command compspecs, lazy
bash-completion loading, native actions, filters, prefix/suffix transforms,
filename and directory modes, globbing, quoting modes, special compspecs,
assignments, and exact `COMP_*` reconstruction. The live PTY test proves
continuous native capture, directory insertion, destructive-edit refresh,
prompt-hook composition, native acceptance, and generation refresh before an
action can observe a Readline-owned edit.

Readline may reserve the terminal's configured erase byte and bypass a
reported `bind -x` registration. Shell Sense wraps the alternate Backspace
binding where Readline permits it and, for every popup action, compares the
actual `READLINE_LINE`/`READLINE_POINT` with the active generation. A mismatch
forces a new native request before navigation or acceptance. This preserves
correctness without changing tty erase settings or claiming that Bash exposes
the same edit-hook fidelity as ZLE or Fish.

Ghost text and partial ghost acceptance are currently a Zsh capability. Fish
and Bash retain their native Right/End bindings; the worker does not advertise
actions their line editors cannot faithfully implement.

### Phase E — Documentation and context adapters (implemented)

- native descriptions retained as menu detail without redundant documentation
  duplication (implemented);
- generation-safe delayed documentation resolution and retained request state
  (implemented);
- a shared Markdown/plain-text parser, Unicode-aware wrapper, responsive
  side/below layout, truncation indicator, and semantic line roles
  (implemented);
- atomic documentation/menu rendering in Zsh, Fish, and Bash with dedicated,
  configurable Blink-inspired styles (implemented);
- bounded native token-context capture from Zsh completion state, Fish's
  tokenizer, and Bash's completion context, published independently of
  candidates for adapter use (implemented);
- daemon-enforced adapter events that can update only metadata on current
  native item IDs, with adversarial role/unknown-ID tests (implemented);
- cancellable per-generation adapter scheduling with validated concurrency,
  deadlines, and bounded subprocess output (implemented);
- zero-I/O Git/systemd kind enrichment and delayed selected-item documentation
  from focused `git -h`, `git log`, `systemctl --help`, and `systemctl show`
  results (implemented);
- generic local-man-page option documentation with focused extraction and
  declaration-aware option matching plus plain-text overstrike removal
  (implemented);
- byte-preserving filesystem resources from Zsh, Fish, and Bash plus ordered,
  argv-only configurable documentation resolvers with one semantic `$value`
  placeholder (implemented);
- weighted, TTL-bound caching for stable help/manual documentation while
  excluding runtime Git-ref and systemd-unit results (implemented);
- adapter cancellation, deadlines, concurrency, and bounded output
  enforcement (implemented).

### Phase F — Blink source (implemented)

- strict newline-delimited JSON presentation endpoint over protocol v7;
- PID-bound live-shell attachment with bounded startup retry and an active
  request/view snapshot;
- external-presentation ownership that suppresses and restores all three
  terminal renderers without erasing candidate state;
- LSP-shaped mapping for label, detail, kind, documentation, edit range,
  stable sort order, source/group identity, and match spans;
- settled-view semantics so Blink receives zero-I/O context enrichment without
  duplicate streamed items;
- lazy documentation resolution and multiline/current-row range translation;
- native selection routing with explicit applied/rejected acknowledgment from
  ZLE, Fish, or Readline;
- proactive Neovim `TermOpen` attachment, cancellation-safe callbacks, and
  terminal-buffer lifecycle cleanup;
- daemon attachment/selection tests, presentation mapping tests, Blink config
  validation, and the shared live Zsh/Fish/Bash regression suite.

### Phase G — hardening and release (implemented)

- release-mode 10,000-candidate ranking benchmark (implemented; 3.45 ms typical
  rank-stage time on the development machine) and request-scoped rank latency
  traces that exclude command buffers (implemented);
- 256-case arbitrary-fragmentation and hostile-byte properties for both wire
  codecs (implemented);
- automatic per-shell worker recovery after a forced crash, verified through
  native-completion PTY tests, plus bounded/escaped hostile-output tests
  (implemented);
- protocol-probed daemon autostart and safe incompatible-daemon replacement
  (implemented);
- embedded, atomic, stale-file-free user installer and XDG-aware per-shell
  initialization output (implemented);
- supported-interface audit for Zsh native capture, followed by removal of the
  empty module/ABI scaffold and its obsolete protocol fields (implemented);
- user documentation and compatibility matrix (implemented).

### Phase H — native conformance and documentation UX (in progress)

This phase is a stabilization gate, not a new candidate source. It turns the
acceptance matrix below into a shared, data-driven contract exercised against
each live shell and completes the documentation pane as an independently
navigable IntelliSense surface.

- build a reusable native-completion conformance harness with identical
  scenarios and assertions for Zsh, Fish, and Bash wherever their public
  completion APIs provide equivalent behavior (implemented; the shared TSV
  covers fuzzy subcommands, long options, option values, and parent, nested,
  quoted, and symlinked directory resources);
- expand coverage to short/combined options, user completions, large or slow
  providers, and further destructive-edit/cancellation cases; native
  descriptions/groups and shell-owned acceptance already have capability and
  live-client coverage;
- record explicitly unsupported shell capabilities instead of weakening a
  shared assertion or silently manufacturing parity (implemented as a
  validated capability matrix);
- retain the complete wrapped documentation model and expose a bounded
  viewport, rather than irreversibly truncating documentation during layout
  (implemented);
- add independent documentation line/page navigation, a manual visibility
  toggle, stable reset rules when selection or generation changes, and
  position metadata for terminal presenters (implemented);
- make all documentation actions configurable and keep candidate navigation
  behavior unchanged (implemented);
- reject a stale or unregistered generation centrally before it can replace or
  navigate the worker's active view, and cover delayed documentation navigation
  in the live Zsh, Fish, and Bash clients (implemented; Bash uses its documented
  action-driven delivery path);
- cover side/below, bordered/borderless, narrow/wide, long Markdown, Unicode,
  first/last page, selection changes, cancellation, and unresolved/empty
  documentation in layout, bridge, and live-shell tests;
- complete the Blink source contract tests for resolve, cancellation, stale
  generations, native acceptance acknowledgement, and terminal lifecycle;
- add release-mode end-to-end latency measurements and request-scoped
  observability for native capture, ranking, enrichment, layout, and render
  delivery;
- finish packaging and release checks only after the conformance and UX gates
  are green on all supported shells.

Implementation order:

1. shared conformance fixture vocabulary and baseline native-shell cases
   (implemented);
2. scrollable documentation model, worker state, actions, and presenters
   (implemented);
3. expanded conformance and adversarial lifecycle cases (in progress);
4. Blink integration contract;
5. latency/observability and packaging gates.

## 15. Acceptance matrix

Each shell must pass native-provider fixtures for:

```text
cd dotfil                 -> native `dotfiles/`
cd dotfiles/nv            -> native `nvim/`
systemctl rstart          -> native `restart`
ls - / ls --recusr        -> native options and available descriptions
custom user completion    -> visible without Shell Sense configuration
spaces, quotes, escapes   -> correct display and shell-owned acceptance
backspace / cursor edit   -> immediate generation replacement
fast typing               -> no stale popup, freeze, or accepted stale item
native file suppression   -> no Shell Sense filesystem candidates appear
```

The project is not cross-shell complete until these are interactive tests,
not only normalization unit tests.
