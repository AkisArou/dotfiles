# Signalbox projects

One folder per project, each a Signalbox application (the SDK lives in
`~/Projects/signalbox/signalbox`). This folder is a pnpm workspace; `sbn` is the
one command for all of them.

| Path                     | What                                                                |
| ------------------------ | ------------------------------------------------------------------- |
| `nts/`                   | NTS: its config, orchestration, status readers, TUI and its pages   |
| `console/`               | the console: every project's pages and notifications in one web app |
| `hosts.json`             | which machine runs which project's host, by hostname                |
| `<project>/project.json` | name, title, roots, API port, whether its coordinator runs          |
| `sbn.ts`                 | the CLI (`~/.local/bin/sbn`)                                        |
| `pnpm-workspace.yaml`    | the workspace, the SDK's injected copy and its patch                |

## sbn

`sbn` finds the project whose root contains the current directory (deepest
root first), the only project when there is one, or `--project <name>`.

```sh
sbn                 # the TUI on the project's running host
sbn web             # the project in the console (http://127.0.0.1:4310/<project>/)
sbn tailnet [off]   # the console and this machine's hosts on your tailnet
sbn status          # the host unit and the project's assignments
sbn start | stop | restart | enable | disable   # signalbox@<project>
sbn logs            # follow the host's log
sbn doctor          # read-only prerequisites
sbn list            # every project and its host
sbn --api ws://127.0.0.1:4321/rpc   # the TUI (or any verb) on another host, e.g. a pilot
```

Any other verb goes to the project's `scripts/cli.ts`. For nts: `sbn show 817`,
`sbn assign 817 [notes]`, `sbn approve 817 [notes]`, `sbn return 817 <notes>`,
`sbn message 817 <text>`, `sbn rework 817 <notes>`, `sbn retry 817`,
`sbn cancel 817 <reason>`: tasks by issue number, through the host's operations.

## The console

One web app for every project: `signalbox-console.service` serves it on this
machine (ports in `console/console.json`), with each project's pages under
`/<project>/` and a switcher between them. It reads `hosts.json`, each project's
`project.json` and the tailnet's names, connects to every project's host (here
on loopback, elsewhere through that machine's tailnet address), and sends this
device's notifications for all of them, its badge counting what waits in all.
`sbn tailnet` puts it at this machine's tailnet address (`/`, `/console/rpc`) with
this machine's hosts (`/<project>/rpc`, `/<project>/terminals`); `sbn hosts sync`
starts it wherever a host runs.

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
