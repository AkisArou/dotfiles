// nts's orchestration: the kit's, with nts's own ways. Its tasks take pool trees
// and land through the landing check, verified by the full check (task-parts.ts);
// its questions are written to GitHub (github.ts); the coordinator also hears of
// landing answers and full-check culprits (wakeup-sources.ts); its agents are
// briefed in nts's terms (briefs.ts).
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { polledSource } from "signalbox/core";
import type { Capability, Readable } from "signalbox/core";
import type { WorkflowStore } from "signalbox/ext/assignments";
import type { AgentToolset } from "signalbox/ext/agents";
import type { Issue } from "@signalbox/kit/shared/protocol";
import type { ManagedAgents } from "@signalbox/kit/orchestration/agents";
import { projectOrchestration } from "@signalbox/kit/orchestration/project";
import type { OrchestrationSettings } from "@signalbox/kit/orchestration/project";
import { fullCheckStatus, landingStatus } from "../status/commands.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { coordinatorInstructions } from "./briefs.ts";
import { gitHubTracker } from "./github.ts";
import type { GitHub } from "./github.ts";
import { ntsTaskParts } from "./task-parts.ts";
import { ntsWakeups } from "./wakeup-sources.ts";

export type { OrchestrationSettings };

/** The repository's area labels (area:<name> in .github/labels.json), which shares must cover. */
function repositoryAreas(root: string): string[] {
  try {
    const spec = JSON.parse(readFileSync(join(root, ".github/labels.json"), "utf8")) as {
      labels?: { name: string }[];
    };
    return (spec.labels ?? [])
      .map((label) => label.name)
      .filter((name) => name.startsWith("area:"))
      .map((name) => name.slice(5));
  } catch {
    return [];
  }
}

export function ntsOrchestration(options: {
  /** The project's name: its records sit under it. */
  readonly project: string;
  readonly root: string;
  readonly directory: string;
  readonly state: Capability<WorkflowStore>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly settings: OrchestrationSettings;
  /** Writes an answered question to its issue (label, comment); tests supply a stand-in. */
  readonly gitHub: GitHub;
  /** Reads the issues again after a label change. */
  readonly refreshIssues: () => Promise<void>;
  /** Tests supply stand-in agents; production uses Claude Code and Codex in tmux. */
  readonly agents?: (toolsets: readonly AgentToolset[]) => ManagedAgents;
}) {
  // The task route and wake-ups watch both, so they stay current; the status pages project them.
  const landing = polledSource<LandingStatus>({
    id: "nts/landing-status",
    interval: 10_000,
    read: (signal) => landingStatus(options.root, signal),
  });
  const fullCheck = polledSource<FullCheckStatus>({
    id: "nts/full-check-status",
    interval: 30_000,
    read: (signal) => fullCheckStatus(options.root, signal),
  });
  const orchestration = projectOrchestration({
    project: options.project,
    title: "NTS",
    root: options.root,
    directory: options.directory,
    state: options.state,
    issues: options.issues,
    settings: options.settings,
    areas: repositoryAreas(options.root),
    tracker: gitHubTracker(options.gitHub),
    refreshIssues: options.refreshIssues,
    parts: ntsTaskParts({ root: options.root, landing: landing.key, fullCheck: fullCheck.key }),
    coordinatorInstructions,
    words: {
      proof: "the focused tests, inner.ts and check.ts --here",
      waits: "a build, inner.ts, a landing check",
      signoff: "the compiler core, runtime/c, the gate or the landing tools",
    },
    wakeups: ntsWakeups({ landing: landing.key, fullCheck: fullCheck.key }),
    extensions: [landing.extension, fullCheck.extension],
    ...(options.agents ? { agents: options.agents } : {}),
  });
  return { ...orchestration, landing: landing.key, fullCheck: fullCheck.key };
}
