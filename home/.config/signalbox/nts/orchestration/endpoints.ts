// The owner's endpoints for orchestration: what the UI shows and what the user
// can change. The gate in host/config.ts decides who may call them.
import { z } from "zod";
import { Cell, mapReadable } from "signalbox/core";
import type { Extension } from "signalbox/core";
import { exposeOperation, exposeSource } from "signalbox/host";
import { assignmentRequest } from "signalbox/ext/assignments/index";
import type { Assignment } from "signalbox/ext/assignments/index";
import type { AgentInstance } from "signalbox/ext/agents/index";
import { textInput } from "signalbox/ext/agents/index";
import { messaging } from "signalbox/ext/agents/features";
import { current } from "./current.ts";
import { sessionOf } from "./activity.ts";
import type { ClaudeActivity } from "./activity.ts";
import * as refs from "../shared/model.ts";
import type { ntsOrchestration } from "./index.ts";
import { harnesses, profileSchema, roles } from "./profiles.ts";
import { taskState } from "./task.ts";

type Orchestration = ReturnType<typeof ntsOrchestration>;

export function assignmentView(assignment: Assignment): refs.AssignmentView {
  const parsed = taskState.safeParse(assignment.state);
  const state = parsed.success ? parsed.data : undefined;
  const issue = /^issue\/(\d+)/.exec(assignment.key);
  return {
    id: assignment.id,
    key: assignment.key,
    status: assignment.status,
    issue: issue ? Number(issue[1]) : null,
    phase: state?.phase ?? null,
    wait: assignment.wait?.reason ?? null,
    help: state?.help ?? null,
    rounds: state?.rounds ?? 0,
    commit: state?.commit ?? null,
    tree: state?.tree ?? null,
    design: state?.design ?? "",
    summary: state?.summary ?? "",
    worker: state?.worker?.id ?? null,
    reviewer: state?.reviewer?.id ?? null,
    error: assignment.error ? `${assignment.error.code}: ${assignment.error.message}` : null,
    acceptedAt: assignment.acceptedAt,
  };
}

/** Managed sessions as Agents rows: role and issue come from the label the routes give them. */
export function managedAgentViews(
  instances: readonly AgentInstance[],
  activity: ClaudeActivity = {},
): refs.Agent[] {
  return instances.map((instance) => ({
    id: instance.id,
    name: instance.label,
    role: /coordinator/i.test(instance.label) ? "coordinator" : /review/i.test(instance.label) ? "reviewer" : "worker",
    // A live session shows what it is doing (busy, idle), not only that it is alive.
    state:
      instance.state === "running" ? (activity[sessionOf(instance.id) ?? ""] ?? instance.state) : instance.state,
    path: null,
    issue: Number(/^#(\d+)/.exec(instance.label)?.[1]) || null,
    terminal: instance.id,
  }));
}

export function orchestrationEndpoints(orchestration: Orchestration): Extension[] {
  const { profiles, work, assignments, decisions, route, agents } = orchestration;
  return [
    exposeSource({
      ref: refs.profileSettings,
      requires: [profiles],
      output: z.custom<refs.ProfileSettingsView>(),
      resolve: (context) => context.get(profiles).settings as unknown as Cell<refs.ProfileSettingsView>,
    }),
    exposeSource({
      ref: refs.assignments,
      requires: [assignments],
      output: z.custom<refs.AssignmentView[]>(),
      resolve: (context) => mapReadable(context.get(assignments), (all) => all.map(assignmentView)),
    }),
    exposeSource({
      ref: refs.userDecisions,
      requires: [decisions],
      output: z.custom<refs.UserDecisionView[]>(),
      resolve: (context) =>
        mapReadable(context.get(decisions).requests, (requests) =>
          requests.map((request) => ({
            id: request.id,
            status: request.status,
            revision: request.revision,
            question: request.input.question,
            context: request.input.context,
            answer: request.result?.decision.answer ?? null,
            requestedAt: request.requestedAt,
          })),
        ),
    }),
    exposeOperation({
      ref: refs.setProfile,
      requires: [profiles],
      input: z.object({ profile: profileSchema, revision: z.number().int() }),
      output: z.object({ revision: z.number() }),
      run: async (input, context) => ({
        revision: (await context.get(profiles).setProfile(input.profile, input.revision)).revision,
      }),
    }),
    exposeOperation({
      ref: refs.setRole,
      requires: [profiles],
      input: z.object({
        role: z.enum(roles),
        profile: z.string(),
        fallback: z.string().optional(),
        revision: z.number().int(),
      }),
      output: z.object({ revision: z.number() }),
      run: async (input, context) => ({
        revision: (
          await context
            .get(profiles)
            .setRole(
              input.role,
              { profile: input.profile, ...(input.fallback ? { fallback: input.fallback } : {}) },
              input.revision,
            )
        ).revision,
      }),
    }),
    exposeOperation({
      ref: refs.setAvailability,
      requires: [profiles],
      input: z.object({
        harness: z.enum(harnesses),
        unavailable: z.object({ reason: z.string().min(1), until: z.number().optional() }).nullable(),
      }),
      output: z.object({ revision: z.number() }),
      run: async (input, context) => ({
        revision: (await context.get(profiles).setAvailability(input.harness, input.unavailable)).revision,
      }),
    }),
    exposeOperation({
      ref: refs.assignTask,
      requires: [work],
      input: z.object({ issue: z.number().int().positive(), instructions: z.string() }),
      output: z.object({ id: z.string() }),
      run: async (input, context) => ({
        id: (
          await context.get(work).submit(
            assignmentRequest(route, { key: `issue/${input.issue}`, input }),
            { requestId: context.requestId },
          )
        ).id,
      }),
    }),
    exposeOperation({
      ref: refs.cancelAssignment,
      requires: [work],
      input: z.object({ assignment: z.string(), reason: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context
          .get(work)
          .cancel({ assignmentId: input.assignment, reason: input.reason, requestId: context.requestId });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.reworkAssignment,
      requires: [work],
      input: z.object({ assignment: z.string(), notes: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(work).report({
          assignmentId: input.assignment,
          data: { kind: "rework", notes: input.notes },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.retryAssignment,
      requires: [work],
      input: z.object({ assignment: z.string() }),
      output: z.object({ ok: z.literal(true) }),
      // Re-runs a held step; its effect recovers from NTS's own state before acting.
      run: async (input, context) => {
        await context.get(work).retry(input.assignment);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.decideDesign,
      requires: [work],
      input: z.object({ assignment: z.string(), accept: z.boolean(), notes: z.string() }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(work).report({
          assignmentId: input.assignment,
          data: { kind: "design_decision", accept: input.accept, notes: input.notes },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.messageWorker,
      requires: [work, agents.runtime],
      input: z.object({ assignment: z.string(), text: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        const assignment = (current(context.get(work).assignments) ?? []).find(
          (candidate) => candidate.id === input.assignment,
        );
        const worker = assignment && assignmentView(assignment).worker;
        if (!worker) throw new Error("That assignment has no current worker");
        const send = context.get(agents.runtime).capability(worker, messaging);
        if (!send) throw new Error("The worker cannot receive messages");
        await send.submit(textInput(`From the user: ${input.text}`), { requestId: context.requestId });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.answerDecision,
      requires: [decisions],
      input: z.object({ id: z.string(), revision: z.number().int(), answer: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(decisions).decide({
          id: input.id,
          expectedRevision: input.revision,
          decision: { answer: input.answer },
          by: { owner: "local" },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
  ];
}
