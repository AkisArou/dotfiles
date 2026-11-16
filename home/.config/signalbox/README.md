# Signalbox projects

One folder per project, each a Signalbox application (the SDK lives in
`~/Projects/signalbox/signalbox`). This folder is a pnpm workspace; `sbn` is the
one command for all of them.

| Path            | What                                                                 |
| --------------- | -------------------------------------------------------------------- |
| `nts/`          | NTS: its config, orchestration, status readers, TUI and browser UI   |
| `<project>/project.json` | name, roots, ports, whether its coordinator runs        |
| `sbn.ts`        | the CLI (`~/.local/bin/sbn`)                                         |
| `pnpm-workspace.yaml` | the workspace, the SDK's injected copy and its patch          |

## sbn

`sbn` finds the project whose root contains the current directory (deepest
root first), the only project when there is one, or `--project <name>`.

```sh
sbn                 # the TUI on the project's running host
sbn web             # the browser UI
sbn status          # the host unit and the project's assignments
sbn start | stop | restart | enable | disable   # signalbox@<project>
sbn logs            # follow the host's log
sbn doctor          # read-only prerequisites
sbn list            # every project and its host
sbn --api ws://127.0.0.1:4321/rpc   # the TUI on another host (a pilot)
```

## Hosts

Each project's host is `signalbox@<project>.service` (a template unit in
`~/.config/systemd/user`). `sbn enable` starts it at login and now. Agent
sessions run in their own systemd scope and outlive host restarts; after a
reboot the host recovers its state and the task routes replace any worker
whose session ended, briefed from the assignment's state.

## A new project

Copy `nts/` as a starting point, give its `project.json` a new name, roots and
ports, add it to nothing else (the workspace includes every folder), run
`pnpm install` here, then `sbn --project <name> start`. State, tmux sockets
and agents are per project root, so two hosts never share them.
