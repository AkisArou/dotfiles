# Signalbox projects

One folder per project, each a Signalbox application (the SDK lives in
`~/Projects/signalbox/signalbox`). This folder is a pnpm workspace; `sbn` is the
one command for all of them.

| Path                     | What                                                                |
| ------------------------ | ------------------------------------------------------------------- |
| `nts/`                   | NTS: its config, orchestration, status readers, TUI and its pages   |
| `nable` (a link)         | Nable FrontEnd: `tools/signalbox` in the nable-solutions repository |
| `console/`               | the console: every project's pages and notifications in one web app |
| `hosts.json`             | which machine runs which project's host, by hostname                |
| `<project>/project.json` | name, title, roots, API port, whether its coordinator runs          |
| `sbn.ts`                 | the CLI (`~/.local/bin/sbn`)                                        |
| `pnpm-workspace.yaml`    | the workspace, the SDK's injected copy and its patch                |
| `links.json`             | projects that live in other repositories: name -> folder            |
| `.tool-versions`         | the Node every project and sbn run on (26), whatever a repo pins    |

## On a new machine

The dotfiles' install (`install/4-config`) turns on lingering (user units run
without a login), installs Node with asdf, clones the SDK to
`~/Projects/signalbox`, links the projects that live elsewhere (`sbn links`),
builds the SDK and installs the projects (`sbn sdk`), and starts this machine's
hosts and the console (`sbn hosts sync`). Then, once, by hand:

1. Clone each linked project's repository where `links.json` says
   (nable-solutions at `~/nable-solutions`), then `sbn update --no-pull`.
2. Copy each project's secrets from a machine that has them, e.g. nable's:
   `scp -rp <machine>:.config/nable ~/.config/ && chmod -R go-rwx ~/.config/nable`.
3. `sbn doctor --project <name>` for each project: every ✗ says how to fix it.
4. Choose this machine's hosts: `sbn hosts here <project...>`, and commit
   `hosts.json`.
5. For the console on your other devices: `sbn tailnet`.

## After pulling: sbn update

`sbn update` is the one command after any change to the dotfiles, the SDK or a
linked project: it fast-forwards those repositories, keeping their local changes
(one whose local changes an incoming commit would overwrite, or with commits of
its own, is left as it is, and said), builds the SDK and installs the projects
(`sbn sdk`: from the pnpm store, fetching only what this machine lacks), reloads
the units, and restarts this machine's hosts and the console (a stopped one stays
stopped). When its pull changes sbn itself, the rest of the update runs on the
new sbn. `sbn update --no-pull` does the same
after pulling by hand. Agents' sessions outlive the restart.

## What runs where

Everything is a user unit, so a sway or i3 session starts nothing for
Signalbox: `signalbox@<project>` for each host `hosts.json` gives this machine,
and `signalbox-console` for the web app. Agents run in tmux, each in its own
systemd scope. To look: `sbn` (the terminal app), `sbn web` (the console),
`sbn logs` (a host's log).

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
sbn update          # pull, build the SDK, install, restart what runs here
sbn sdk             # build the SDK and install the projects (no pull, no restart)
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

In the project's git repository, `sbn init` makes it one: the kit's git project
(`@signalbox/kit/project/git`) in the repository's `.signalbox/` folder, linked here
(`links.json`) and installed. Its tasks are this machine's board (`sbn task add`)
unless `--tasks github` (the repository's issues); a reviewed change is pushed as it
is unless `--check "<command>"` names what it must pass first; `--setup` overrides
the install found from the lockfile. Everything it chose is in its `project.json`.
With `--global` the folder is `~/.config/signalbox/<name>` instead, kept with these
dotfiles (which are public: its project.json names the checkout and its commands).
The coordinator runs (it writes, assigns and steers the tasks, on the board or on
GitHub) unless `--no-coordinator`. Then `sbn doctor`, `sbn hosts here <projects...>`,
and commit the folder. Claude Code must trust the checkout once (run `claude` there): each task's
worktree inherits it.

A project with ways of its own (nts, nable) assembles the kit's parts itself: copy
`nts/` as a starting point, give its `project.json` a new name, roots and ports, run
`sbn sdk`, then `sbn --project <name> start`. A project kept in its own repository
goes in `links.json` (`sbn links` links it and lists it in the workspace); run its
scripts with `node --run`, since pnpm inside another repository takes that
repository's workspace. State, tmux sockets and agents are per project root, so two
hosts never share them.
