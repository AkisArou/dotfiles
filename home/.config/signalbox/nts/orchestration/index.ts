// Composes NTS orchestration from its parts (the general ones from the kit). Wiring order matters only for the
// late references the tools hold; everything else is declared by capability.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { latestValue, polledSource } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Assignment, WorkflowStore } from "signalbox/ext/assignments";
import { assignmentService } from "signalbox/ext/assignments";
import { decisionRequests } from "signalbox/ext/decisions";
import { openWorkflowSqlite } from "signalbox/ext/integrations/openworkflow";
import type { Issue } from "@signalbox/kit/shared/protocol";
import { fullCheckStatus, landingStatus } from "../status/commands.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { managedAgents } from "@signalbox/kit/orchestration/agents";
import type { ManagedAgents } from "@signalbox/kit/orchestration/agents";
import type { AgentToolset } from "signalbox/ext/agents";
import { coordinator } from "./coordinator.ts";
import { claudeMessagingProbe, profiles } from "@signalbox/kit/orchestration/profiles";
import { nextLeavesOnStart, priorities } from "@signalbox/kit/orchestration/priorities";
import { capacityFollowsSettings, settingsFile } from "@signalbox/kit/orchestration/settings";
import { shareRank } from "@signalbox/kit/orchestration/shares";

import { taskRoute } from "@signalbox/kit/orchestration/task";
import { ntsTaskParts } from "./task-parts.ts";
import { coordinatorTools, Late, reviewerTools, workerTools } from "./tools.ts";
import { needs, questionAnswer, questionInput } from "./needs.ts";
import type { GitHub } from "./needs.ts";
import type { Wiring } from "./tools.ts";
import { wakeupFeed } from "./wakeups.ts";

export interface OrchestrationSettings {
  /** Install the coordinator and its wake-ups. Off, the routes still run assignments submitted by hand. */
  readonly coordinator: boolean;
  /** settings.json: worker slots, rounds and each role's models, the only source for them. */
  readonly file: string;
}

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
  const engine = openWorkflowSqlite({ path: join(options.directory, "workflows.sqlite") });
  const file = settingsFile({ path: options.settings.file, areas: repositoryAreas(options.root) });
  // The issues' areas, for the worker shares; read when a slot is given out.
  // Undefined until the issues are read: the shares cannot rank before.
  let issuesNow: () => readonly Issue[] | undefined = () => undefined;
  const taskAreas: Extension = {
    id: "nts-task-areas",
    requires: [options.issues],
    start(context) {
      const issues = context.get(options.issues);
      issuesNow = () => latestValue(issues);
    },
  };
  const issueOf = (assignment: Assignment) => (assignment.input as { issue?: number }).issue;
  const placeOf = (assignment: Assignment) => {
    const issue = issuesNow()?.find((candidate) => candidate.number === issueOf(assignment));
    return issue && { area: issue.area, stream: issue.streamNumber };
  };
  const profileStore = profiles({
    project: options.project,
    state: options.state,
    settings: file.service,
  });
  const urgency = priorities({ project: options.project, state: options.state });
  const settingsNow = file.current;
  const assignmentList = new Late<Capability<Readable<readonly Assignment[]>>>();
  const decisions = decisionRequests({
    id: "nts-user",
    state: options.state,
    input: questionInput,
    decision: questionAnswer,
  });
  const waiting = needs({
    state: options.state,
    questions: decisions.service,
    issues: options.issues,
    gitHub: options.gitHub,
    refreshIssues: options.refreshIssues,
  });
  // Routes watch both, so they stay current; the dashboard's pages project them.
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
  const wiring: Wiring = {
    work: new Late(),
    route: new Late(),
    coordinator: new Late(),
    mailbox: new Late(),
    issues: options.issues,
    decisions: decisions.service,
    priorities: urgency.service,
    settings: file.service,
    refreshIssues: options.refreshIssues,
  };
  const tools = {
    worker: workerTools(wiring),
    reviewer: reviewerTools(wiring),
    coordinator: coordinatorTools(wiring),
  };
  const toolsets = [tools.worker, tools.reviewer, tools.coordinator];
  const agents = options.agents
    ? options.agents(toolsets)
    : managedAgents({
        project: options.project,
        state: options.state,
        directory: options.directory,
        toolsets,
      });
  const route = taskRoute({
    project: options.project,
    issues: options.issues,
    parts: ntsTaskParts({ root: options.root, landing: landing.key, fullCheck: fullCheck.key }),
    profiles: profileStore.service,
    assignments: () => assignmentList.get(),
    agents,
    workerTools: tools.worker,
    reviewerTools: tools.reviewer,
    maxRounds: () => settingsNow().maxRounds,
  });
  wiring.route.set(route);
  const work = assignmentService({
    id: "nts-work",
    state: options.state,
    engine: engine.engine,
    routes: [route],
    capacity: (resource) => (resource === "worker" ? settingsNow().workerSlots : 1),
    // A freed worker slot goes to the group furthest below its share; within a
    // group (and for files), to the most urgent waiting task, then the oldest.
    // Until the issues are read, no area is known: the order waits (undefined) rather
    // than ranking every task by urgency alone, across the shares.
    priority: (assignment, all) =>
      issuesNow() === undefined
        ? undefined
        : // The owner's next-slot list first, in its order, whatever the shares.
          (urgency.jump(issueOf(assignment)) ??
          shareRank({
            shares: settingsNow().shares,
            assignment,
            all,
            placeOf,
            urgency: urgency.rank(issueOf(assignment)),
          })),
  });
  wiring.work.set(work.service);
  assignmentList.set(work.assignments);
  const lead = options.settings.coordinator
    ? coordinator({
        root: options.root,
        state: options.state,
        engine: engine.engine,
        agents,
        profiles: profileStore.service,
        settings: file.service,
        needs: waiting.view,
        issues: options.issues,
        work: work.service,
        tools: tools.coordinator,
      })
    : undefined;
  if (lead) {
    wiring.coordinator.set(lead.service);
    wiring.mailbox.set(lead.mailbox);
  }
  return {
    route,
    work: work.service,
    assignments: work.assignments,
    status: work.status,
    profiles: profileStore.service,
    settings: file.service,
    priorities: urgency.service,
    decisions: decisions.service,
    needs: waiting.view,
    issues: options.issues,
    refreshIssues: options.refreshIssues,
    agents,
    landing: landing.key,
    fullCheck: fullCheck.key,
    coordinator: lead?.service,
    extensions: [
      engine.extension,
      // Before the assignment service, so its first admission reads the file's values.
      file.extension,
      taskAreas,
      profileStore.extension,
      urgency.extension,
      // Probe only the real Claude Code; stand-in agents have nothing to probe.
      ...(options.agents ? [] : [claudeMessagingProbe(options.root, profileStore.service)]),
      decisions.extension,
      waiting.extension,
      landing.extension,
      fullCheck.extension,
      ...agents.extensions,
      work.extension,
      capacityFollowsSettings(file.service, work.service),
      nextLeavesOnStart(urgency.service, work.assignments),
      ...(lead
        ? [
            ...lead.extensions,
            wakeupFeed({
              state: options.state,
              mailbox: lead.mailbox,
              work: work.service,
              landing: landing.key,
              fullCheck: fullCheck.key,
              decisions: decisions.service,
              needs: waiting.view,
            }),
          ]
        : []),
    ],
  };
}
