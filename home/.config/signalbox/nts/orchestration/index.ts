// Composes NTS orchestration from its parts. Wiring order matters only for the
// late references the tools hold; everything else is declared by capability.
import { join } from "node:path";
import { latestValue, polledSource } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import { poll } from "signalbox/core/utils";
import type { Assignment, Assignments, WorkflowStore } from "signalbox/ext/assignments";
import { assignmentService } from "signalbox/ext/assignments";
import { decisionRequests } from "signalbox/ext/decisions";
import { openWorkflowSqlite } from "signalbox/ext/integrations/openworkflow";
import type { Issue } from "../shared/model.ts";
import { fullCheckStatus, landingStatus } from "../status/commands.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { managedAgents } from "./agents.ts";
import type { ManagedAgents } from "./agents.ts";
import type { AgentToolset } from "signalbox/ext/agents";
import { coordinator } from "./coordinator.ts";
import { profiles } from "./profiles.ts";
import type { Profiles } from "./profiles.ts";
import { priorities } from "./priorities.ts";
import { settingsFile } from "./settings.ts";
import type { SettingsFile } from "./settings.ts";
import { probeClaudeMessaging } from "signalbox/ext/integrations/claude";
import { taskRoute } from "./task.ts";
import {
  coordinatorTools,
  Late,
  reviewerTools,
  userAnswer,
  userQuestion,
  workerTools,
} from "./tools.ts";
import type { Wiring } from "./tools.ts";
import { wakeupFeed } from "./wakeups.ts";

/**
 * Claude Code's messaging rests on CLI features that an upgrade can change. The
 * host checks them at start and every few hours; a failure marks the harness
 * unavailable with the exact reason, and a later pass clears only that mark.
 */
function claudeMessagingProbe(root: string, profilesKey: Capability<Profiles>): Extension {
  const PREFIX = "messaging probe failed: ";
  return {
    id: "nts-claude-messaging-probe",
    requires: [profilesKey],
    start(context) {
      const store = context.get(profilesKey);
      const check = async () => {
        const result = await probeClaudeMessaging({ cwd: root, signal: context.signal });
        const marked = latestValue(store.unavailable)?.claude?.reason.startsWith(PREFIX);
        if (!result.ok) await store.setAvailability("claude", { reason: PREFIX + result.reason });
        else if (marked) await store.setAvailability("claude", null);
      };
      context.afterStart(() => {
        const loop = poll(check, 6 * 3_600_000, context.signal, (error) =>
          context.log.error("claude probe", error),
        );
        context.onStop(() => loop);
      });
    },
  };
}

export interface OrchestrationSettings {
  /** Install the coordinator and its wake-ups. Off, the routes still run assignments submitted by hand. */
  readonly coordinator: boolean;
  /** settings.json: worker slots, rounds and each role's models, the only source for them. */
  readonly file: string;
}

/** A raised worker count wakes the waiting tasks, whoever raised it (a client or a hand edit). */
function capacityFollowsSettings(
  settings: Capability<SettingsFile>,
  work: Capability<Assignments>,
): Extension {
  return {
    id: "nts-capacity-follows-settings",
    requires: [settings, work],
    start(context) {
      const file = context.get(settings);
      let slots = file.current().workerSlots;
      const stop = file.reading.subscribe(() => {
        const next = file.current().workerSlots;
        if (next > slots)
          void context
            .get(work)
            .capacityChanged(["worker"])
            .catch((error) => context.log.error("capacity", error));
        slots = next;
      });
      context.onStop(stop);
    },
  };
}

export function ntsOrchestration(options: {
  readonly root: string;
  readonly directory: string;
  readonly state: Capability<WorkflowStore>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly settings: OrchestrationSettings;
  /** Tests supply stand-in agents; production uses Claude Code and Codex in tmux. */
  readonly agents?: (toolsets: readonly AgentToolset[]) => ManagedAgents;
}) {
  const engine = openWorkflowSqlite({ path: join(options.directory, "workflows.sqlite") });
  const file = settingsFile({ path: options.settings.file });
  const profileStore = profiles({ state: options.state, settings: file.service });
  const urgency = priorities({ state: options.state });
  const settingsNow = file.current;
  const assignmentList = new Late<Capability<Readable<readonly Assignment[]>>>();
  const decisions = decisionRequests({
    id: "nts-user",
    state: options.state,
    input: userQuestion,
    decision: userAnswer,
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
  };
  const tools = {
    worker: workerTools(wiring),
    reviewer: reviewerTools(wiring),
    coordinator: coordinatorTools(wiring),
  };
  const toolsets = [tools.worker, tools.reviewer, tools.coordinator];
  const agents = options.agents
    ? options.agents(toolsets)
    : managedAgents({ state: options.state, directory: options.directory, toolsets });
  const route = taskRoute({
    root: options.root,
    issues: options.issues,
    landing: landing.key,
    fullCheck: fullCheck.key,
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
    // Freed slots and files go to the most urgent waiting task, then the oldest.
    priority: (assignment) => urgency.rank((assignment.input as { issue?: number }).issue),
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
    agents,
    landing: landing.key,
    fullCheck: fullCheck.key,
    coordinator: lead?.service,
    extensions: [
      engine.extension,
      // Before the assignment service, so its first admission reads the file's values.
      file.extension,
      profileStore.extension,
      urgency.extension,
      // Probe only the real Claude Code; stand-in agents have nothing to probe.
      ...(options.agents ? [] : [claudeMessagingProbe(options.root, profileStore.service)]),
      decisions.extension,
      landing.extension,
      fullCheck.extension,
      ...agents.extensions,
      work.extension,
      capacityFollowsSettings(file.service, work.service),
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
            }),
          ]
        : []),
    ],
  };
}
