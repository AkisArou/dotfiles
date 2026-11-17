// The coordinator: one Claude Code session that Signalbox keeps alive across
// replacements, with a durable mailbox for its wake-ups. Each generation starts
// from a short brief and its predecessor's checkpoint and queries the rest.
import type { Capability, Readable } from "signalbox/core";
import { latestValue } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { AgentToolset } from "signalbox/ext/agents/tools";
import type { Assignments, WorkflowStore } from "signalbox/ext/assignments/index";
import type { DurableEngine } from "signalbox/ext/assignments/engine";
import { agentMailbox, agentService } from "signalbox/ext/orchestration/services";
import type { Issue } from "../shared/model.ts";
import type { ManagedAgents } from "./agents.ts";
import { coordinatorInstructions } from "./briefs.ts";
import type { Profiles } from "./profiles.ts";

export function coordinator(options: {
  readonly root: string;
  readonly state: Capability<WorkflowStore>;
  readonly engine: Capability<DurableEngine>;
  readonly agents: ManagedAgents;
  readonly profiles: Capability<Profiles>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly work: Capability<Assignments>;
  readonly tools: AgentToolset;
}) {
  const service = agentService({
    id: "nts-coordinator",
    runtime: options.agents.runtime,
    state: options.state,
    engine: options.engine,
    replicas: 1,
    requires: [options.profiles, options.issues, options.work],
    // Small on purpose: the coordinator reads details through its tools.
    async bootstrap(context) {
      const assignments = latestValue(context.get(options.work).assignments) ?? [];
      const issues = latestValue(context.get(options.issues)) ?? [];
      return {
        generation: context.generation,
        checkpoint: context.checkpoint ?? null,
        openIssues: issues.length,
        needsUser: issues
          .filter((issue) => issue.labels.includes("state:needs-user"))
          .map((issue) => issue.number),
        activeTasks: assignments
          .filter((assignment) => assignment.status === "active" || assignment.status === "queued")
          .map((assignment) => ({
            id: assignment.id,
            key: assignment.key,
            wait: assignment.wait?.reason ?? null,
          })),
      } satisfies Json;
    },
    async launch(context) {
      const resolved = await context.get(options.profiles).resolve("coordinator");
      if ("wait" in resolved)
        return {
          available: false,
          reason: resolved.wait,
          ...(resolved.retryAt === undefined ? {} : { retryAt: resolved.retryAt }),
        };
      const instance = await options.agents.start(
        context.get(options.agents.runtime),
        {
          profile: resolved.profile,
          label: `NTS coordinator (generation ${context.generation})`,
          cwd: options.root,
          instructions: coordinatorInstructions(),
          brief: [
            `You are coordinator generation ${context.generation}.`,
            "Your starting state (your predecessor's checkpoint and the current summary):",
            JSON.stringify(context.bootstrap ?? {}, null, 2),
            "Call pending_messages, handle each, acknowledge it, and save_checkpoint after decisions.",
          ].join("\n"),
          tools: [options.tools],
          requestId: context.requestId,
        },
        context.signal,
      );
      return { available: true, instance };
    },
    // Replace a session only when it has proven to end; an unobservable one holds.
    restart: ({ instance }) =>
      instance.state === "ended" || instance.state === "exited"
        ? { allowed: true }
        : { allowed: false, reason: `Coordinator session is ${instance.state}` },
  });
  const inbox = agentMailbox({
    id: "nts-coordinator-inbox",
    state: options.state,
    runtime: options.agents.runtime,
    target: service.target,
  });
  return {
    service: service.service,
    target: service.target,
    mailbox: inbox.mailbox,
    extensions: [service.extension, inbox.extension],
  };
}
