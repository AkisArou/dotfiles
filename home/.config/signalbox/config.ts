// This is the NTS application's configuration. Signalbox contains no NTS policy.
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";
import { ntsStateDirectory } from "./host/state-directory.ts";
import { expandPath } from "signalbox/core/utils";
import type { CommandSpec } from "signalbox/ext/execution/index";
import type { TerminalTarget } from "signalbox/ext/terminals/index";

const directory = fileURLToPath(new URL("./", import.meta.url));
const path = (value: string) => resolve(expandPath(value));
const root = path(process.env.NTS_ROOT ?? "~/Projects/nts");
export interface NtsConfig {
  readonly root: string;
  readonly repo: string;
  readonly poolRoot: string;
  readonly plan: string;
  readonly issueLimit: number;
  readonly issueIntervalMs: number;
  readonly state: string;
  readonly webPort: number;
  readonly apiPort: number;
  readonly origin?: string;
  readonly apiUrl?: string;
  readonly terminalPrograms: readonly (CommandSpec & { id: string; label: string })[];
  readonly discoverTerminals: boolean;
  readonly attachedTerminals: readonly {
    id: string;
    label: string;
    path: string | null;
    target: TerminalTarget;
  }[];
}
export const ntsConfig: NtsConfig = {
  root,
  repo: process.env.NTS_GITHUB_REPO ?? "AkisArou/nts",
  poolRoot: path(process.env.NTS_POOL_ROOT ?? "~/.cache/nts-pool"),
  plan: "docs/plan.md",
  issueLimit: 400,
  issueIntervalMs: 120_000,
  // Reuse existing execution state in place; fresh installs use XDG_STATE_HOME.
  state: ntsStateDirectory({
    appDirectory: directory,
    projectRoot: root,
    override: process.env.SIGNALBOX_NTS_STATE,
  }),
  webPort: Number(process.env.SIGNALBOX_NTS_PORT ?? 4310),
  apiPort: Number(process.env.SIGNALBOX_NTS_API_PORT ?? 4311),
  // Only local clients are accepted. Remote deployment supplies its own gate/proxy.
  origin: process.env.SIGNALBOX_NTS_ORIGIN,
  apiUrl: process.env.SIGNALBOX_NTS_API_URL,
  // Native CLI flags belong here; launch is always an explicit user action.
  terminalPrograms: [
    { id: "codex", label: "Codex", command: "codex", args: [] },
    { id: "claude", label: "Claude Code", command: "claude", args: [] },
    { id: "shell", label: "Shell", command: process.env.SHELL ?? "/bin/sh", args: [] },
  ],
  // Existing Codex/Claude panes in the default tmux server under these roots
  // are discovered read-only. This never starts, stops, or adopts the agents.
  discoverTerminals: true,
  // Use tmuxTerminal({target, pane, server}) for nondefault tmux servers,
  // or supply any other TerminalTarget. Targets stay on the host.
  attachedTerminals: [],
};
