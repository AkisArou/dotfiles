# NTS application

The NTS configuration and clients for Signalbox. This application reads your **real NTS checkout**, **AkisArou/nts GitHub issues**, and NTS's status commands. It has no simulator, seeded task board or sample-data mode.

## Run on your NTS desktop

Requires **Node 26.4+**, pnpm and a GitHub CLI login with access to `AkisArou/nts`. Native terminals require tmux and installed/authenticated Codex or Claude Code. The TUI finder uses fzf; the Git view uses lazygit.

From the Signalbox repository root:

```sh
pnpm install
pnpm run doctor
pnpm dev
```

Open **http://127.0.0.1:4310**. In another terminal:

```sh
pnpm tui
```

The default checkout is `~/Projects/nts`, and the default pool is `~/.cache/nts-pool`. Change [`config.ts`](config.ts), or set `NTS_ROOT`, `NTS_POOL_ROOT` and `NTS_GITHUB_REPO`. GitHub access uses `gh`'s existing login or `GH_TOKEN`. The issue reader needs a recent `gh` that supports the `parent` and `subIssuesSummary` JSON fields, matching NTS’s issue hierarchy conventions.

The prerequisite command only inspects the checkout, tool availability and GitHub access. It does not start NTS services or agents. The server rejects a missing checkout with an actionable error. Individual reader failures stay visible, retaining the last good reading when available.

## Configuration and ownership

| Path        | Responsibility                                                                                            |
| ----------- | --------------------------------------------------------------------------------------------------------- |
| `config.ts` | Checkout, GitHub repository, pool, polling, ports, persistent state and native program/attachment choices |
| `host/`     | NTS source wiring, GitHub issue mapping and native terminal catalog/discovery                             |
| `nts/`      | NTS tool contracts, status readers, benchmark/conformance projections and health thresholds               |
| `shared/`   | Application DTOs, endpoint references and shared React hooks                                              |
| `tui/`      | OpenTUI components, terminal palette, Vim navigation, fzf filters and lazygit                             |
| `web/`      | React DOM components, Astryx Neutral theme, mobile navigation and installable PWA                         |

The SDK lives in [`../signalbox/`](../signalbox/README.md). NTS labels, streams, thresholds and tooling are application code, not Signalbox defaults.

GitHub tasks preserve stream/group ancestry, sub-issue progress, assignees, `agent:*` owners and textual blockers. The plan orders streams. `state:needs-user` issues appear in **Needs you**; `in-progress`, `state:held` and `state:parked` retain their existing meanings. Empty groups and unscoped issues stay visible. Parent stream issues and the coordinator's `signalbox:state` issue are excluded from the task list. Pool and landing observations retain their claims on issues, mark ready work as in progress, and show its workspace/owner when known. These are observed NTS claims, not new Signalbox assignments. The configured issue limit is checked explicitly rather than silently truncating the board.

**Landing**, **Full check**, **Benchmarks**, **Conformance**, **Machine** and **Health** read NTS's existing tooling. **Plan** reads `docs/plan.md`. NTS owns its worktree pool and landing service. This dashboard does not install an automatic dispatcher, start a coordinator, claim tasks or merge changes. Existing coordinators and workers continue using their native tools and communication; connecting autonomous coordination is a separate integration step.

## Native agents

In **Agents**, press **n** in the TUI or choose **New Codex / New Claude Code / New Shell** in the browser. Commands and argv are configured in `config.ts`; they run in the NTS checkout only after your explicit action.

Existing foreground Codex/Claude panes in the default tmux server are discovered under the checkout and pool roots. This is attachment discovery, not conversation/session import. Wrappers or nondefault tmux servers can be attached explicitly using `config.ts`'s `attachedTerminals` array and the SDK's `tmuxTerminal({ target, pane, server })` adapter, or any other `TerminalTarget`. Explicit attachments should not also be selected by discovery; disable `discoverTerminals` if you manage the whole list yourself.

Views start read-only. **Enter** in the TUI or **Take input** in the browser requests exclusive input. **Ctrl+]** releases input. Native keys and paste, including Ctrl+C, go to the agent while it owns input. Reconnection returns read-only and never replays input. Closing the viewer or stopping the host detaches; it does not terminate the native program. Sessions persist through host restart, but not necessarily a reboot or tmux-server failure.

Native terminal transport uses a live channel, not dashboard snapshots or saved transcripts. Harness conversations, skills, permissions and memory remain owned by Codex/Claude and your existing setup.

## TUI controls

The TUI inherits your terminal foreground, background, transparency and ANSI palette. The browser independently uses Astryx's default Neutral theme. The TUI works inside or outside tmux.

| Keys                                     | Behavior                                               |
| ---------------------------------------- | ------------------------------------------------------ |
| `1`–`9`, `0`, Tab/Shift+Tab or `gt`/`gT` | Switch views, retaining selections                     |
| `j`/`k`, arrows                          | Move in a list or scroll focused details               |
| Ctrl+D/U                                 | Half a focused pane; accounts for row height           |
| Ctrl+F/B, PageDown/Up                    | Full focused pane                                      |
| `gg`/`G`, Home/End                       | First/last item or top/bottom of content               |
| Enter/right, Esc/left/`h`                | Details, then back to the list                         |
| `/`                                      | Find in the current list with fzf                      |
| `f`, `F`                                 | Select task filters; clear filters                     |
| `l`, `L`                                 | lazygit in the selected workspace; in the NTS checkout |
| `?`, `q` / Ctrl+Q                        | Help; quit the viewer                                  |

Filters support area, stream, kind, state, labels and ownership. Values within a facet are OR; different facets are AND. Tool argv and facets live in `tui/config.ts`. fzf inherits your environment and color options. Local tools run on the TUI machine, so their workspace paths must exist there.

## State and deployment

Fresh state lives under `$XDG_STATE_HOME/signalbox/nts/<project-id>` (default `~/.local/state/signalbox/nts/<project-id>`), independent of the application source checkout. Existing NTS installations reuse `v2/playground/.signalbox/nts/` in place to preserve native executor receipts and tmux socket identity. Old `nts-lab` sandbox state is left intact and is not loaded as NTS. `SIGNALBOX_NTS_STATE` overrides the location. Stop an old host before starting its replacement; only one host may own a state directory. See [migration notes](../signalbox/docs/migration.md) before moving existing state.

`SIGNALBOX_NTS_PORT` and `SIGNALBOX_NTS_API_PORT` override ports 4310 and 4311. `SIGNALBOX_NTS_API_URL` selects the TUI API or public browser API; `SIGNALBOX_NTS_ORIGIN` selects the browser origin behind a proxy. Listeners and the application gate currently accept local connections. A VPS deployment needs deliberate proxy/access configuration in `host/config.ts`. PWA installation works on localhost or HTTPS; it caches the application shell, not task mutations or terminal input.

```sh
pnpm --dir playground check
pnpm --dir playground test
pnpm --dir playground test:tui
```

Tests use isolated data under test-only fixtures. No tests modify your NTS checkout, GitHub issues or existing agent sessions.
