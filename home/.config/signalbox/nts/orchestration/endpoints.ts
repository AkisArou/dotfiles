// The owner's endpoints for orchestration: what the UI shows and what the user
// can change. The gate in host/config.ts decides who may call them.
import { z } from "zod";
import { combineReadables, latestValue, mapReadable } from "signalbox/core";
import type { Extension } from "signalbox/core";
import { exposeOperation, exposeSource } from "signalbox/host";
import { assignmentRequest } from "signalbox/ext/assignments";
import type { Assignment } from "signalbox/ext/assignments";
import type { AgentInstance } from "signalbox/ext/agents";
import * as refs from "../shared/model.ts";
import type { ntsOrchestration } from "./index.ts";
import { catalog, harnesses, profileFromId, profileId, profileLabel, roles } from "./models.ts";
import type { Profile } from "./models.ts";
import { picks } from "./settings.ts";
import { checkTaskModel } from "./tools.ts";
import { answerFor, issueStates } from "./needs.ts";
import { terminate } from "signalbox/ext/agents";
import { taskState } from "./task-model.ts";
import { urgencyValue } from "./priorities.ts";

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
    phaseSince: state?.phaseSince ?? null,
    progress: state?.progress?.note ?? null,
    progressUntil: state?.progress?.until ?? null,
    stalled: state?.stalled ?? null,
    escalation: state?.escalation ?? null,
    note: state?.note ?? "",
    design: state?.design ?? "",
    summary: state?.summary ?? "",
    worker: state?.worker?.id ?? null,
    reviewer: state?.reviewer?.id ?? null,
    error: assignment.error ? `${assignment.error.code}: ${assignment.error.message}` : null,
    acceptedAt: assignment.acceptedAt,
  };
}

/** Managed sessions as Agents rows: role and issue come from the label the routes give them. */
/**
 * Managed sessions as Agents rows. A task's worker or reviewer works in its
 * task's tree, so that is its path (lazygit and the list use it); the
 * coordinator works in the project root.
 */
export function managedAgentViews(
  instances: readonly AgentInstance[],
  assignments: readonly Assignment[] = [],
  root: string | null = null,
): refs.Agent[] {
  const tasks = assignments.flatMap((assignment) => {
    const state = taskState.safeParse(assignment.state);
    return assignment.route === "nts/task" && state.success
      ? [
          {
            issue: (assignment.input as { issue?: number }).issue,
            state: state.data,
            at: assignment.acceptedAt,
          },
        ]
      : [];
  });
  return instances.map((instance) => {
    const issue = Number(/^#(\d+)/.exec(instance.label)?.[1]) || null;
    const coordinator = /coordinator/i.test(instance.label);
    // Its own task first (it is that task's worker or reviewer now), else the issue's latest task.
    const task =
      tasks.find(
        ({ state }) => state.worker?.id === instance.id || state.reviewer?.id === instance.id,
      ) ??
      tasks
        .filter((candidate) => candidate.issue === issue && candidate.state.tree)
        .sort((a, b) => b.at - a.at)[0];
    return {
      id: instance.id,
      name: instance.label,
      role: coordinator ? "coordinator" : /review/i.test(instance.label) ? "reviewer" : "worker",
      // A live Claude session's state is what it is doing (busy, idle); see the SDK's ManagedDriver.activity.
      state: instance.state,
      path: coordinator ? root : (task?.state.tree ?? null),
      issue,
      terminal: instance.id,
    };
  });
}

const profileView = (profile: Profile): refs.ProfileView => ({
  ...profile,
  id: profileId(profile),
  label: profileLabel(profile),
});

export function orchestrationEndpoints(orchestration: Orchestration): Extension[] {
  const {
    profiles,
    work,
    assignments,
    decisions,
    route,
    settings,
    status,
    needs: needsView,
  } = orchestration;
  return [
    exposeSource({
      ref: refs.settings,
      requires: [settings, status, profiles],
      output: z.custom<refs.SettingsView>(),
      resolve: (context) =>
        combineReadables(
          [context.get(settings).reading, context.get(status), context.get(profiles).unavailable],
          (reading, admission, unavailable): refs.SettingsView => ({
            path: reading.path,
            version: reading.version,
            error: reading.error,
            workerSlots: reading.settings.workerSlots,
            maxRounds: reading.settings.maxRounds,
            roles: Object.fromEntries(
              roles.map((role) => [
                role,
                {
                  pick: reading.settings.roles[role].pick,
                  models: reading.settings.roles[role].models.map(profileView),
                },
              ]),
            ) as refs.SettingsView["roles"],
            paused: admission.paused ? { reason: admission.reason ?? "paused" } : null,
            unavailable,
            catalog: catalog.map(profileView),
          }),
        ),
    }),
    exposeOperation({
      ref: refs.setSettings,
      requires: [settings],
      input: z.object({
        workerSlots: z.number().int().optional(),
        maxRounds: z.number().int().optional(),
        roles: z
          .partialRecord(
            z.enum(roles),
            z.object({ pick: z.enum(picks).optional(), models: z.array(z.string()).optional() }),
          )
          .optional(),
        version: z.string().optional(),
      }),
      output: z.object({ version: z.string() }),
      run: async ({ version, workerSlots, maxRounds, roles: changed = {} }, context) => {
        const reading = await context.get(settings).update(
          (current) => ({
            ...current,
            ...(workerSlots === undefined ? {} : { workerSlots }),
            ...(maxRounds === undefined ? {} : { maxRounds }),
            roles: Object.fromEntries(
              roles.map((role) => {
                const change = changed[role];
                const now = current.roles[role];
                return [
                  role,
                  {
                    pick: change?.pick ?? now.pick,
                    models: change?.models ? change.models.map(profileFromId) : now.models,
                  },
                ];
              }),
            ),
          }),
          version,
        );
        return { version: reading.version };
      },
    }),
    exposeOperation({
      ref: refs.pauseTasks,
      requires: [work],
      input: z.object({ reason: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(work).pauseNewAssignments(input.reason);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.resumeTasks,
      requires: [work],
      input: z.object({}),
      output: z.object({ ok: z.literal(true) }),
      run: async (_input, context) => {
        await context.get(work).resumeNewAssignments();
        return { ok: true as const };
      },
    }),
    exposeSource({
      ref: refs.assignments,
      requires: [assignments],
      output: z.custom<refs.AssignmentView[]>(),
      resolve: (context) => mapReadable(context.get(assignments), (all) => all.map(assignmentView)),
    }),
    exposeSource({
      ref: refs.needs,
      requires: [needsView],
      output: z.custom<refs.NeedsView>(),
      resolve: (context) => context.get(needsView),
    }),
    exposeOperation({
      ref: refs.setAvailability,
      requires: [profiles],
      input: z.object({
        harness: z.enum(harnesses),
        unavailable: z
          .object({ reason: z.string().min(1), until: z.number().optional() })
          .nullable(),
      }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(profiles).setAvailability(input.harness, input.unavailable);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.prioritizeTask,
      requires: [orchestration.priorities],
      input: z.object({ issue: z.number().int().positive(), urgency: urgencyValue }),
      output: z.object({ urgency: z.record(z.string(), z.number()) }),
      run: async (input, context) => ({
        urgency: {
          ...(await context.get(orchestration.priorities).set(input.issue, input.urgency)),
        },
      }),
    }),
    ...(orchestration.coordinator
      ? [
          exposeOperation({
            ref: refs.replaceCoordinator,
            requires: [orchestration.coordinator],
            input: z.object({ reason: z.string().min(1) }),
            output: z.object({ ok: z.literal(true) }),
            run: async (input, context) => {
              await context.get(orchestration.coordinator!).replace({
                reason: input.reason,
                requestId: context.requestId,
              });
              return { ok: true as const };
            },
          }),
        ]
      : []),
    exposeOperation({
      ref: refs.endAgent,
      requires: [orchestration.agents.runtime],
      input: z.object({ id: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        const end = context.get(orchestration.agents.runtime).capability(input.id, terminate);
        if (!end) throw new Error(`No managed agent ${input.id} can be ended`);
        await end.terminate(context.signal);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.signOff,
      requires: [work],
      input: z.object({ assignment: z.string(), accept: z.boolean(), notes: z.string() }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(work).report({
          assignmentId: input.assignment,
          data: { kind: "signoff", accept: input.accept, notes: input.notes },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.assignTask,
      requires: [work, settings],
      input: z.object({
        issue: z.number().int().positive(),
        instructions: z.string(),
        model: z.string().optional(),
      }),
      output: z.object({ id: z.string() }),
      run: async (input, context) => {
        if (input.model !== undefined) checkTaskModel(context.get(settings).current(), input.model);
        const { id } = await context
          .get(work)
          .submit(assignmentRequest(route, { key: `issue/${input.issue}`, input }), {
            requestId: context.requestId,
          });
        return { id };
      },
    }),
    exposeOperation({
      ref: refs.cancelAssignment,
      requires: [work],
      input: z.object({ assignment: z.string(), reason: z.string().min(1) }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(work).cancel({
          assignmentId: input.assignment,
          reason: input.reason,
          requestId: context.requestId,
        });
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
      requires: [work],
      // The owner may stand in for the coordinator; the worker sees who spoke.
      input: z.object({
        assignment: z.string(),
        text: z.string().min(1),
        from: z.enum(["user", "coordinator"]).default("user"),
      }),
      output: z.object({ ok: z.literal(true) }),
      // Through the route: delivered once, recorded, and it answers the worker's open question.
      run: async (input, context) => {
        await context.get(work).report({
          assignmentId: input.assignment,
          data: { kind: "message", from: input.from ?? "user", text: input.text },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.answerQuestion,
      requires: [decisions],
      input: z.union([
        z.object({ id: z.string(), revision: z.number().int(), option: z.number().int().min(0) }),
        z.object({
          id: z.string(),
          revision: z.number().int(),
          answer: z.string().min(1),
          then: z.enum(issueStates).optional(),
        }),
      ]),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        const questions = context.get(decisions);
        const request = await questions.read(input.id);
        if (!request || request.status !== "pending")
          throw new Error(`${input.id} is not an open question`);
        await questions.decide({
          id: input.id,
          expectedRevision: input.revision,
          decision: answerFor(request.input, input),
          by: { owner: "local" },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.withdrawQuestion,
      requires: [decisions],
      input: z.object({ id: z.string(), revision: z.number().int() }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(decisions).withdraw(input.id, input.revision);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.answerIssue,
      requires: [decisions, needsView],
      input: z.object({
        issue: z.number().int().positive(),
        answer: z.string().min(1),
        then: z.enum(issueStates),
      }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        const waiting = latestValue(context.get(needsView))?.unasked.find(
          (issue) => issue.number === input.issue,
        );
        if (!waiting) throw new Error(`#${input.issue} is not waiting on you without a question`);
        // The owner's own question, decided at once: the answer still posts and moves the issue.
        const questions = context.get(decisions);
        const request = await questions.request({
          id: `owner/issue-${input.issue}/${context.requestId}`,
          input: {
            question: `What should happen with #${input.issue} ${waiting.title}?`,
            context: "",
            issue: input.issue,
          },
        });
        await questions.decide({
          id: request.id,
          expectedRevision: request.revision,
          decision: answerFor(request.input, input),
          by: { owner: "local" },
          requestId: context.requestId,
        });
        return { ok: true as const };
      },
    }),
  ];
}
