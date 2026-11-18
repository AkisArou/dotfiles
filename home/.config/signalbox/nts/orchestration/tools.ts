// The tools each role's session is granted. Every handler authenticates its
// caller from the bridge's trusted context against live assignment state, so a
// model cannot speak for another session by naming it.
import { z } from "zod";
import type { Capability, Readable } from "signalbox/core";
import { latestValue } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { toJson } from "signalbox/core/utils";
import { agentTool, agentToolset } from "signalbox/ext/agents";
import type { AgentToolContext } from "signalbox/ext/agents";
import { assignmentRequest } from "signalbox/ext/assignments";
import type { Assignment, Assignments } from "signalbox/ext/assignments";
import type { AgentService, Mailbox } from "signalbox/ext/orchestration";
import type { Issue } from "@signalbox/kit/shared/protocol";
import { toolInput } from "./agents.ts";
import type { TaskRoute } from "./task.ts";
import { questionInput } from "./needs.ts";
import type { Questions } from "./needs.ts";
import { profileId, profileLabel } from "./models.ts";
import type { Settings, SettingsFile } from "./settings.ts";
import type { TaskState } from "./task-model.ts";
import { urgencyValue } from "./priorities.ts";
import type { Priorities } from "./priorities.ts";
import { taskState } from "./task-model.ts";
import { configuredCodexModel } from "./codex-config.ts";

/** A capability that exists only once later wiring creates it; checked on use. */
export class Late<T> {
  #value: T | undefined;
  set(value: T): void {
    if (this.#value !== undefined) throw new Error("Late wiring was set twice");
    this.#value = value;
  }
  get(): T {
    if (this.#value === undefined) throw new Error("Late wiring was used before it was set");
    return this.#value;
  }
}

export interface Wiring {
  readonly work: Late<Capability<Assignments>>;
  readonly route: Late<TaskRoute>;
  readonly coordinator: Late<Capability<AgentService>>;
  readonly mailbox: Late<Capability<Mailbox>>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly decisions: Capability<Questions>;
  readonly priorities: Capability<Priorities>;
  readonly settings: Capability<SettingsFile>;
  /** Reads the issues now: an issue filed a moment ago is not in the last reading yet. */
  readonly refreshIssues: () => Promise<void>;
}

/** Makes sure the issues reading has the issue before a task for it is submitted. */
export async function knownIssue(
  issues: Readable<Issue[]>,
  refresh: () => Promise<void>,
  issue: number,
): Promise<void> {
  if (!(latestValue(issues) ?? []).some((open) => open.number === issue)) await refresh();
}

/** A task may name a worker model only when workers pick "coordinator", and only a listed one. */
export function checkTaskModel(settings: Settings, model: string): void {
  const workers = workerModels(settings);
  if (workers.pick !== "coordinator")
    throw new Error(
      `Workers pick "${workers.pick}" (the owner's setting), so a task cannot name a model; leave model out`,
    );
  if (!workers.models.some((listed) => listed.id === model))
    throw new Error(
      `${model} is not one of the workers' models: ${workers.models.map((listed) => listed.id).join(", ")}`,
    );
}

/** The workers' models as the coordinator sees them: how they are picked, and their ids. */
export function workerModels(settings: Settings) {
  const { pick, models } = settings.roles.worker;
  return {
    pick,
    models: models.map((profile) => ({
      id: profileId(profile),
      label: profileLabel(profile, configuredCodexModel()),
    })),
  };
}

const ok = z.object({ ok: z.literal(true) });
/** Tool results built from our own state; the bridge still checks they are JSON. */
const json = z.custom<Json>((value) => value !== undefined);
const tasks = (context: AgentToolContext, wiring: Wiring) =>
  latestValue(context.get(wiring.work.get()).assignments) ?? [];
const taskOf = (assignment: Assignment): TaskState | undefined => {
  const parsed = taskState.safeParse(assignment.state);
  return parsed.success ? parsed.data : undefined;
};

/** The active task whose current worker (or reviewer) is the caller. */
function callerTask(context: AgentToolContext, wiring: Wiring, role: "worker" | "reviewer") {
  for (const assignment of tasks(context, wiring)) {
    if (assignment.route !== "nts/task" || assignment.status !== "active") continue;
    const state = taskOf(assignment);
    if (state?.[role]?.id === context.caller.instanceId) return { assignment, state };
  }
  throw new Error(`This session is not the ${role} of an active NTS task`);
}

/** Say what you wait for: the task expects you idle until then and does not nudge you. */
function progressTool(wiring: Wiring, role: "worker" | "reviewer") {
  return agentTool({
    name: "report_progress",
    description:
      "Before you wait on background work (a build, inner.ts, a landing check), say what you wait for and the minutes it needs; the task then leaves you idle until then instead of nudging you.",
    input: toolInput(
      z.object({ note: z.string().min(1), minutes: z.number().int().min(1).max(480) }),
    ),
    output: ok,
    async run(input, context) {
      const { assignment } = callerTask(context, wiring, role);
      await context.get(wiring.work.get()).report(
        {
          assignmentId: assignment.id,
          data: { kind: "progress", role, ...input },
          requestId: context.requestId,
        },
        context.signal,
      );
      return { ok: true as const };
    },
  });
}

export function workerTools(wiring: Wiring) {
  const report =
    (kind: "design" | "ready" | "help") => async (data: Json, context: AgentToolContext) => {
      const { assignment } = callerTask(context, wiring, "worker");
      await context.get(wiring.work.get()).report(
        {
          assignmentId: assignment.id,
          data: { kind, ...(data as object) },
          requestId: context.requestId,
        },
        context.signal,
      );
      return { ok: true as const };
    };
  return agentToolset({
    id: "nts-worker",
    tools: [
      agentTool({
        name: "report_design",
        description:
          "Send your design for approval: the cause, the change at that cause, the files and the controls that prove it.",
        input: toolInput(z.object({ summary: z.string().min(1) })),
        output: ok,
        run: report("design"),
      }),
      agentTool({
        name: "report_ready",
        description:
          "Report the proved, committed change: the exact commit (40 hex) and what the focused tests, inner.ts and check.ts --here showed.",
        input: toolInput(
          z.object({ commit: z.string().regex(/^[0-9a-f]{40}$/), summary: z.string().min(1) }),
        ),
        output: ok,
        run: report("ready"),
      }),
      progressTool(wiring, "worker"),
      agentTool({
        name: "ask_coordinator",
        description: "Ask the coordinator for a decision or help; it answers by message.",
        input: toolInput(z.object({ message: z.string().min(1) })),
        output: ok,
        run: report("help"),
      }),
    ],
  });
}

export function reviewerTools(wiring: Wiring) {
  return agentToolset({
    id: "nts-reviewer",
    tools: [
      progressTool(wiring, "reviewer"),
      agentTool({
        name: "report_review",
        description:
          "Report your review of the exact commit once: accept, or changes with each finding as file:line, what breaks and the input that shows it.",
        input: toolInput(
          z.object({
            commit: z.string().regex(/^[0-9a-f]{40}$/),
            verdict: z.enum(["accept", "changes"]),
            findings: z.string(),
            disputesDesign: z.boolean().default(false),
          }),
        ),
        output: ok,
        async run(input, context) {
          const { assignment, state } = callerTask(context, wiring, "reviewer");
          if (state.reviewer!.commit !== input.commit)
            throw new Error(`You review ${state.reviewer!.commit}`);
          await context.get(wiring.work.get()).report(
            {
              assignmentId: assignment.id,
              data: { kind: "review", ...input },
              requestId: context.requestId,
            },
            context.signal,
          );
          return { ok: true as const };
        },
      }),
    ],
  });
}

export function coordinatorTools(wiring: Wiring) {
  /** Runs a consequential action only while the caller is the current coordinator generation. */
  const authorized = <T>(context: AgentToolContext, action: () => Promise<T>) =>
    context.get(wiring.coordinator.get()).withAuthority(context.caller.instanceId, action);
  const summary = (assignment: Assignment) => {
    const state = taskOf(assignment);
    return {
      id: assignment.id,
      key: assignment.key,
      status: assignment.status,
      phase: state?.phase ?? null,
      wait: assignment.wait?.reason ?? null,
      help: state?.help ?? null,
      rounds: state?.rounds ?? 0,
      commit: state?.commit ?? null,
      tree: state?.tree ?? null,
      design: state?.design ?? "",
    };
  };
  return agentToolset({
    id: "nts-coordinator",
    tools: [
      agentTool({
        name: "list_tasks",
        description:
          "Open NTS issues, newest board reading: number, title, area, stream, kind and state labels.",
        input: toolInput(z.object({ area: z.string().optional(), stream: z.string().optional() })),
        output: json,
        async run(input, context) {
          const issues = latestValue(context.get(wiring.issues)) ?? [];
          return toJson(
            issues
              .filter(
                (issue) =>
                  (!input.area || issue.area === input.area) &&
                  (!input.stream || issue.stream === input.stream),
              )
              .map((issue) => ({
                number: issue.number,
                title: issue.title,
                area: issue.area,
                stream: issue.stream,
                state: issue.state,
                labels: issue.labels,
                blockedBy: issue.blockedBy,
              })),
          );
        },
      }),
      agentTool({
        name: "read_task",
        description: "One issue's card and labels.",
        input: toolInput(z.object({ issue: z.number().int().positive() })),
        output: json,
        async run(input, context) {
          const issue = (latestValue(context.get(wiring.issues)) ?? []).find(
            (issue) => issue.number === input.issue,
          );
          if (!issue) throw new Error(`Issue #${input.issue} is not on the board`);
          return toJson(issue);
        },
      }),
      agentTool({
        name: "list_assignments",
        description:
          "NTS task assignments: phase, what each waits for, help requests, rounds, commit and tree.",
        input: toolInput(z.object({ active: z.boolean().default(true) })),
        output: json,
        async run(input, context) {
          return toJson(
            tasks(context, wiring)
              .filter(
                (assignment) =>
                  !input.active || assignment.status === "active" || assignment.status === "queued",
              )
              .map(summary),
          );
        },
      }),
      agentTool({
        name: "assign_task",
        description:
          "Start one fresh worker on an issue (key issue/<N>; a second attempt after a revert uses attempt 2). " +
          "When workers pick 'coordinator', model names one of the workers' models (see workerModels in your starting state, or the error) for this task: a harder task can take a stronger model, a mechanical one a cheaper one. Without it the first available is used.",
        input: toolInput(
          z.object({
            issue: z.number().int().positive(),
            instructions: z.string().default(""),
            attempt: z.number().int().min(1).default(1),
            model: z.string().optional(),
          }),
        ),
        output: json,
        run: (input, context) =>
          authorized(context, async () => {
            if (input.model !== undefined)
              checkTaskModel(context.get(wiring.settings).current(), input.model);
            await knownIssue(context.get(wiring.issues), wiring.refreshIssues, input.issue);
            return toJson(
              await context.get(wiring.work.get()).submit(
                assignmentRequest(wiring.route.get(), {
                  key:
                    input.attempt === 1
                      ? `issue/${input.issue}`
                      : `issue/${input.issue}/${input.attempt}`,
                  input: {
                    issue: input.issue,
                    instructions: input.instructions,
                    ...(input.model === undefined ? {} : { model: input.model }),
                  },
                }),
                { requestId: context.requestId, signal: context.signal },
              ),
            );
          }),
      }),
      agentTool({
        name: "decide_design",
        description: "Approve a worker's design or return it with specific reasons.",
        input: toolInput(
          z.object({ assignment: z.string(), accept: z.boolean(), notes: z.string() }),
        ),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context.get(wiring.work.get()).report(
              {
                assignmentId: input.assignment,
                data: { kind: "design_decision", accept: input.accept, notes: input.notes },
                requestId: context.requestId,
              },
              context.signal,
            );
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "sign_off",
        description:
          "Sign off a reviewed change to the compiler core, runtime/c, the gate or the landing tools after reading it: accept queues the exact commit; refuse sends it back to build with your notes.",
        input: toolInput(
          z.object({ assignment: z.string(), accept: z.boolean(), notes: z.string() }),
        ),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context.get(wiring.work.get()).report(
              {
                assignmentId: input.assignment,
                data: { kind: "signoff", accept: input.accept, notes: input.notes },
                requestId: context.requestId,
              },
              context.signal,
            );
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "rework",
        description:
          "Send a held or wrong landing back to build: its queue entry is withdrawn and the worker gets your notes.",
        input: toolInput(z.object({ assignment: z.string(), notes: z.string().min(1) })),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context.get(wiring.work.get()).report(
              {
                assignmentId: input.assignment,
                data: { kind: "rework", notes: input.notes },
                requestId: context.requestId,
              },
              context.signal,
            );
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "message_worker",
        description:
          "Send a message to a task's current worker. It answers the worker's open question; an escalated task returns to build with it.",
        input: toolInput(z.object({ assignment: z.string(), text: z.string().min(1) })),
        output: ok,
        // Through the route: delivered once, recorded, and it answers the worker's open question.
        run: (input, context) =>
          authorized(context, async () => {
            await context.get(wiring.work.get()).report(
              {
                assignmentId: input.assignment,
                data: { kind: "message", from: "coordinator", text: input.text },
                requestId: context.requestId,
              },
              context.signal,
            );
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "prioritize_task",
        description:
          "Set a task's urgency (-9 to 9, default 0; higher first): freed worker slots and files go to the most urgent waiting task, then the oldest.",
        input: toolInput(z.object({ issue: z.number().int().positive(), urgency: urgencyValue })),
        output: json,
        run: (input, context) =>
          authorized(context, async () => ({
            urgency: await context.get(wiring.priorities).set(input.issue, input.urgency),
          })),
      }),
      agentTool({
        name: "cancel_assignment",
        description:
          "Cancel a task assignment; its sessions end, its tree and branch stay with the pool.",
        input: toolInput(z.object({ assignment: z.string(), reason: z.string().min(1) })),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context.get(wiring.work.get()).cancel(
              {
                assignmentId: input.assignment,
                reason: input.reason,
                requestId: context.requestId,
              },
              context.signal,
            );
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "ask_user",
        description:
          "Ask the owner a question that is theirs to decide (deletions, product direction, spending, outward PRs, scope beyond a task's card). " +
          "State the decision in one sentence; put what they need to decide it in context. When it is about an issue, name the issue: it is labelled state:needs-user until answered, and the answer is posted on it. " +
          "When the answer is a choice, give options, each with then: where the issue goes after it (ready, parked or held). Use a stable id per question.",
        input: toolInput(questionInput.extend({ id: z.string().min(1) })),
        output: json,
        run: (input, context) =>
          authorized(context, async () => {
            const { id, ...question } = input;
            if (
              question.issue !== undefined &&
              !(latestValue(context.get(wiring.issues)) ?? []).some(
                (issue) => issue.number === question.issue,
              )
            )
              throw new Error(`#${question.issue} is not an open issue`);
            return toJson(await context.get(wiring.decisions).request({ id, input: question }));
          }),
      }),
      agentTool({
        name: "withdraw_question",
        description:
          "Withdraw your open question to the owner when it became moot or you will ask it better (with its issue and options). It leaves Needs you.",
        input: toolInput(z.object({ id: z.string().min(1) })),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            const questions = context.get(wiring.decisions);
            const request = await questions.read(input.id);
            if (!request || request.status !== "pending")
              throw new Error(`${input.id} is not an open question`);
            await questions.withdraw(input.id, request.revision);
            return { ok: true as const };
          }),
      }),
      agentTool({
        name: "save_checkpoint",
        description:
          "Save what your successor needs: open decisions, order of work, what you are waiting for and why.",
        input: toolInput(z.object({ checkpoint: z.string().min(1) })),
        output: ok,
        async run(input, context) {
          await context.get(wiring.coordinator.get()).checkpoint({
            instanceId: context.caller.instanceId,
            data: { checkpoint: input.checkpoint, at: Date.now() },
            requestId: context.requestId,
          });
          return { ok: true as const };
        },
      }),
      agentTool({
        name: "pending_messages",
        description: "Wake-up messages not yet acknowledged.",
        input: toolInput(z.object({})),
        output: json,
        async run(_input, context) {
          return toJson(
            (await context.get(wiring.mailbox.get()).pending())
              .filter((message) => !message.acknowledged)
              .map((message) => ({ id: message.id, input: message.input })),
          );
        },
      }),
      agentTool({
        name: "acknowledge",
        description: "Acknowledge a wake-up message once you have handled it.",
        input: toolInput(z.object({ id: z.string().min(1) })),
        output: ok,
        async run(input, context) {
          await context
            .get(wiring.mailbox.get())
            .acknowledge({ id: input.id, instanceId: context.caller.instanceId });
          return { ok: true as const };
        },
      }),
    ],
  });
}
