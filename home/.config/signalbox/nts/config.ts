// This is the NTS application's configuration. Signalbox contains no NTS policy.
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { projectStateDirectory } from "@signalbox/kit/host/state-directory";
import { expandPath } from "signalbox/core/utils";
import type { OrchestrationSettings } from "./orchestration/index.ts";
import type { AttachedTerminal, TerminalProgram } from "@signalbox/kit/host/terminals";

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
  /** Other addresses that proxy to this host, whose pages may use it. This machine's
   * tailnet address (tailscale serve) is read from Tailscale and needs no entry. */
  publicOrigins?: string[];
};
const path = (value: string) => resolve(expandPath(value));
const root = path(process.env.NTS_ROOT ?? project.roots[0]!);
export interface NtsConfig {
  /** The project's name (project.json): where its records sit, and what it is called. */
  readonly name: string;
  readonly root: string;
  readonly repo: string;
  readonly poolRoot: string;
  readonly plan: string;
  readonly issueLimit: number;
  readonly issueIntervalMs: number;
  readonly state: string;
  readonly webPort: number;
  readonly apiPort: number;
  /** Pages that may talk to the host beyond its loopback addresses (a proxy's address). */
  readonly publicOrigins: readonly string[];
  readonly apiUrl?: string;
  readonly terminalPrograms: readonly TerminalProgram[];
  readonly discoverTerminals: boolean;
  readonly orchestration: OrchestrationSettings;
  readonly attachedTerminals: readonly AttachedTerminal[];
}
export const ntsConfig: NtsConfig = {
  name: project.name,
  root,
  repo: process.env.NTS_GITHUB_REPO ?? "AkisArou/nts",
  poolRoot: path(process.env.NTS_POOL_ROOT ?? "~/.cache/nts-pool"),
  plan: "docs/plan.md",
  issueLimit: 400,
  issueIntervalMs: 120_000,
  // One state directory per project root, under XDG_STATE_HOME.
  state: projectStateDirectory({
    project: project.name,
    projectRoot: root,
    override: process.env.SIGNALBOX_NTS_STATE ?? project.state,
  }),
  webPort: Number(process.env.SIGNALBOX_NTS_PORT ?? project.webPort),
  apiPort: Number(process.env.SIGNALBOX_NTS_API_PORT ?? project.apiPort),
  // Only local clients are accepted. Remote deployment supplies its own gate/proxy.
  publicOrigins: [
    ...(project.publicOrigins ?? []),
    ...(process.env.SIGNALBOX_NTS_ORIGIN ? [process.env.SIGNALBOX_NTS_ORIGIN] : []),
  ],
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
    // Worker slots, rounds and each role's models: settings.json is their only
    // source. Edit it by hand or from the TUI, web view or sbn; the host applies it live.
    file: fileURLToPath(new URL("./settings.json", import.meta.url)),
  },
};
