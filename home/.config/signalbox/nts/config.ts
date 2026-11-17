// This is the NTS application's configuration. Signalbox contains no NTS policy.
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { ntsStateDirectory } from "./host/state-directory.ts";
import { expandPath } from "signalbox/core/utils";
import type { CommandSpec } from "signalbox/ext/execution/index";
import type { TerminalTarget } from "signalbox/ext/terminals/index";
import type { OrchestrationSettings } from "./orchestration/index.ts";

/** The project's identity, shared with sbn: its roots, ports and coordinator choice. */
const project = JSON.parse(readFileSync(new URL("./project.json", import.meta.url), "utf8")) as {
  name: string;
  roots: string[];
  webPort: number;
  apiPort: number;
  coordinator: boolean;
  /** Where this project's state lives, when not the per-root default. Agents' tmux
   * sessions and execution receipts are tied to it, so it moves only while no agent runs. */
  state?: string;
};
const path = (value: string) => resolve(expandPath(value));
const root = path(process.env.NTS_ROOT ?? project.roots[0]!);
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
  readonly orchestration: OrchestrationSettings;
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
  // One state directory per project root, under XDG_STATE_HOME.
  state: ntsStateDirectory({
    projectRoot: root,
    override: process.env.SIGNALBOX_NTS_STATE ?? project.state,
  }),
  webPort: Number(process.env.SIGNALBOX_NTS_PORT ?? project.webPort),
  apiPort: Number(process.env.SIGNALBOX_NTS_API_PORT ?? project.apiPort),
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
  orchestration: {
    // project.json decides; SIGNALBOX_NTS_COORDINATOR=0 or 1 overrides it for one host.
    coordinator:
      process.env.SIGNALBOX_NTS_COORDINATOR === undefined
        ? project.coordinator
        : process.env.SIGNALBOX_NTS_COORDINATOR === "1",
    workerSlots: 2,
    maxRounds: 4,
    // Initial profiles for a fresh state. Later changes live in the host's state
    // (Agents view or the profiles operations) and need no restart.
    profiles: {
      profiles: [
        { id: "opus-high", label: "Claude Opus 5.5, high effort", harness: "claude", model: "claude-opus-5-5", effort: "high" },
        { id: "opus-medium", label: "Claude Opus 5.5, medium effort", harness: "claude", model: "claude-opus-5-5", effort: "medium" },
        { id: "codex", label: "Codex, configured model", harness: "codex" },
      ],
      roles: {
        coordinator: { profile: "opus-high" },
        worker: { profile: "opus-high" },
        reviewer: { profile: "opus-high" },
      },
    },
  },
};
