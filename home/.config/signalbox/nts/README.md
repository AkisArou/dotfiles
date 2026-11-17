# NTS on Signalbox

The NTS application on the [Signalbox SDK](../../../../Projects/signalbox/signalbox/README.md). It reads the real NTS checkout, the `AkisArou/nts` GitHub issues and NTS's status commands, hosts native agent sessions, and orchestrates NTS's task workflow. NTS policy lives here; the SDK supplies mechanisms only.

## Run

Requires Node 26.4+, pnpm, tmux, a GitHub CLI login with access to `AkisArou/nts`, and an authenticated Claude Code (Codex optional). From anywhere inside `~/Projects/nts` or a pool tree:

```sh
sbn enable          # the host (signalbox@nts) at login, and now
sbn                 # the TUI
sbn web             # the browser UI, http://127.0.0.1:4310
sbn status          # host, and each task's phase, time in it and what it waits for
sbn show 817        # one task: design, latest report, commit
sbn approve 817     # steer by issue number: approve, return, message, rework, retry, cancel
sbn doctor          # read-only prerequisites
```

Settings: [`project.json`](project.json) holds the roots, ports (4310 browser, 4311 API) and whether the coordinator runs; [`config.ts`](config.ts) holds the rest (pool, polling, terminal programs, initial profiles). Environment variables override for one host: `NTS_ROOT`, `SIGNALBOX_NTS_PORT`, `SIGNALBOX_NTS_API_PORT`, `SIGNALBOX_NTS_STATE`, `SIGNALBOX_NTS_COORDINATOR=0|1`. State lives in `~/.local/state/signalbox/nts/<project-id>` unless `project.json`'s `state` names another directory; one host owns it at a time. Agents' tmux sessions and execution receipts are tied to that directory, so it moves only while no agent runs.

`pnpm sdk` rebuilds the SDK and refreshes the workspace's injected copy; `pnpm check` type-checks; `pnpm test` and `pnpm test:tui` run the tests, which use isolated fixtures and never touch the NTS checkout, GitHub or running agents.

## Layout

| Path             | Responsibility                                                                     |
| ---------------- | ---------------------------------------------------------------------------------- |
| `project.json`   | The project's identity, shared with sbn: roots, ports, coordinator                 |
| `config.ts`      | Settings: pool, polling, state, terminal programs and initial profiles             |
| `host/`          | Host wiring: access gate, GitHub issue mapping, native terminals and discovery     |
| `status/`        | NTS status contracts and readers: landing, full check, pool, machine, records      |
| `orchestration/` | Agent profiles, the task and review routes, the coordinator, its tools and wakeups |
| `shared/`        | DTOs and endpoint references shared by the host and both clients                   |
| `tui/`, `web/`   | The OpenTUI and React DOM clients                                                  |

## Orchestration

NTS's workflow (AGENTS.md, `docs/workflow.md`) is enforced by durable routes, not by an agent's memory. Signalbox records each accepted assignment, its phase and every external action, so a host restart or a power cut resumes where it stopped and never repeats an action whose outcome is unknown.

**Profiles** are live data, not code. A profile names a harness (`claude` or `codex`), a model, an effort and extra arguments. Each role (`coordinator`, `worker`, `reviewer`) selects a profile. Profiles and role selections are stored in the host's state and changed through the `profiles.set` and `roles.set` operations (UI or coordinator tool), with no restart. A route resolves its role's profile when it launches an agent and records the resolved profile with the launch, so recovery relaunches the same thing. A running agent keeps its profile; a change applies to the next launch. `config.ts` supplies the initial profiles.

**One fresh agent per task.** The `nts/task` route owns one GitHub issue, keyed `issue/<N>`. Its admission requires an open task issue that is not `state:needs-user`, `state:held` or `state:parked`. It reserves one `worker` slot, and one `file:<path>` resource per path in the card's **Paths:** line, so two tasks never edit one file concurrently. Its phases:

1. **workspace**: `pool.ts take --issue N`, recovered from `pool.ts status --json` by the task's branch.
2. **design**: a worker launched in the tree with the issue card and the rules; it reports a design.
3. **approve**: the coordinator accepts or returns the design through its tools.
4. **build**: the worker implements and proves the change (focused tests, `inner.ts`, the landing check `--here`), then reports the commit and its record.
5. **review**: a fresh reviewer agent (`nts/review`, keyed by commit) reads the diff and the evidence and reports accept or findings. Findings return to build.
6. **queue**: one `check.ts <commit>` submission per commit, recovered from `check.ts status --json`.
7. **land**: a refusal returns to build with its failing steps and logs; a hold waits.
8. **verify**: the full check's `verified` reaches the commit; the worker is retired and the assignment finishes.

**The coordinator** is one Claude Code session maintained by `agentService`. Each generation starts from a short brief, its last accepted checkpoint and a small status summary; it queries details through tools rather than receiving every issue up front. Its tools submit and steer assignments, approve designs, message workers, save checkpoints and ask the user. It has no tool for anything the user decides: deletions of shared state, product direction, spending or outward pull requests go to the user as decision requests (**Needs you**).

**Wakeups are events**, never a timer. Worker reports, review verdicts, landing answers, full-check verdicts and reverts, `verified` advances and machine holds reach the coordinator's durable mailbox. Until NTS publishes its event journal (#815), these are derived from changes in NTS's status outputs.

**Usage.** A harness whose usage is exhausted is marked unavailable; routes launching that role wait or use the role's fallback profile, and `pauseNewAssignments` stops admission when no harness is available.

**Sessions outlive the host.** Managed sessions run in a tmux server of their own (`sb-agents-<id>`), started in a transient systemd scope, so stopping or restarting the host (including a host running as a systemd unit) leaves every session running. A session whose tmux server or session tmux reports gone reads as ended, and the task route replaces its worker with a fresh one briefed from the assignment's state: the design, the last commit and the latest instruction.

**Messages to Claude sessions.** Signalbox's Claude sessions bypass permission prompts (the user's settings), and Claude Code holds any cross-session message to such a session unless the sender identifies as bypassing. Signalbox therefore starts its own sessions with `--settings '{"crossSessionInbound":"accept"}'`. This applies to those sessions only; it means any local Claude session of this user can message them. The host checks at start that the installed Claude Code still offers `claude agents --json` and `claude mcp serve`'s `ListAgents` and `SendMessage`, and marks Claude unavailable with the reason when it does not.

## Native agents and terminals

In **Agents**, launch a Codex, Claude Code or shell session on request (`n` in the TUI). Existing foreground Codex/Claude panes in the default tmux server under the checkout and pool roots are discovered read-only. Views start read-only; **Enter** (TUI) or **Take input** (browser) takes exclusive input and **Ctrl+]** releases it. Closing a viewer or stopping the host detaches; it never terminates a native program.

## TUI keys

| Keys                                     | Behavior                                               |
| ---------------------------------------- | ------------------------------------------------------ |
| `1`–`9`, `0`, Tab/Shift+Tab or `gt`/`gT` | Switch views, retaining selections                     |
| `j`/`k`, arrows                          | Move in a list or scroll focused details               |
| Ctrl+D/U, Ctrl+F/B                       | Half or full focused pane                              |
| `gg`/`G`                                 | First/last item or top/bottom of content               |
| Enter/right, Esc/left/`h`                | Details, then back to the list                         |
| `/`                                      | Find in the current list with fzf                      |
| `f`, `F`                                 | Select task filters; clear filters                     |
| `e` (Agents)                             | Show or hide ended sessions                            |
| `l`, `L`                                 | lazygit in the selected workspace; in the NTS checkout |
| `?`, `q` / Ctrl+Q                        | Help; quit the viewer                                  |
