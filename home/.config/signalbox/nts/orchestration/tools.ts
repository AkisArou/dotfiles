// The tools each role's session is granted. Every handler authenticates its
// caller from the bridge's trusted context against live assignment state, so a
// model cannot speak for another session by naming it.
import { z } from "zod";
import type { Capability, Readable } from "signalbox/core";
import { latestValue } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { toJson } from "signalbox/core/utils";
import { agentTool, agentToolset } from "signalbox/ext/agents/tools";
import type { AgentToolContext } from "signalbox/ext/agents/tools";
import { assignmentRequest } from "signalbox/ext/assignments/index";
import type { Assignment, Assignments } from "signalbox/ext/assignments/index";
import type { Decisions } from "signalbox/ext/decisions/index";
import type { Mailbox, AgentService } from "signalbox/ext/orchestration/services";
import type { Issue } from "../shared/model.ts";
import { toolInput } from "./agents.ts";
import type { TaskRoute, TaskState } from "./task.ts";
import { taskState } from "./task.ts";

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

export const userQuestion = z.object({ question: z.string().min(1), context: z.string() });
export const userAnswer = z.object({ answer: z.string().min(1) });
export type UserDecisions = Decisions<z.infer<typeof userQuestion>, z.infer<typeof userAnswer>>;

export interface Wiring {
  readonly work: Late<Capability<Assignments>>;
  readonly route: Late<TaskRoute>;
  readonly coordinator: Late<Capability<AgentService>>;
  readonly mailbox: Late<Capability<Mailbox>>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly decisions: Capability<UserDecisions>;
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
      await context
        .get(wiring.work.get())
        .report(
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
      await context
        .get(wiring.work.get())
        .report(
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
          await context
            .get(wiring.work.get())
            .report(
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
          "Start one fresh worker on an issue (key issue/<N>; a second attempt after a revert uses attempt 2).",
        input: toolInput(
          z.object({
            issue: z.number().int().positive(),
            instructions: z.string().default(""),
            attempt: z.number().int().min(1).default(1),
          }),
        ),
        output: json,
        run: (input, context) =>
          authorized(context, async () =>
            toJson(
              await context.get(wiring.work.get()).submit(
                assignmentRequest(wiring.route.get(), {
                  key:
                    input.attempt === 1
                      ? `issue/${input.issue}`
                      : `issue/${input.issue}/${input.attempt}`,
                  input: { issue: input.issue, instructions: input.instructions },
                }),
                { requestId: context.requestId, signal: context.signal },
              ),
            ),
          ),
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
        name: "rework",
        description:
          "Send a held or wrong landing back to build: its queue entry is withdrawn and the worker gets your notes.",
        input: toolInput(z.object({ assignment: z.string(), notes: z.string().min(1) })),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context
              .get(wiring.work.get())
              .report(
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
        description: "Send a message to a task's current worker.",
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
        name: "cancel_assignment",
        description:
          "Cancel a task assignment; its sessions end, its tree and branch stay with the pool.",
        input: toolInput(z.object({ assignment: z.string(), reason: z.string().min(1) })),
        output: ok,
        run: (input, context) =>
          authorized(context, async () => {
            await context
              .get(wiring.work.get())
              .cancel(
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
          "Ask the user a question that is theirs to decide (deletions, product direction, spending, outward PRs). Use a stable id per question.",
        input: toolInput(
          z.object({ id: z.string().min(1), question: z.string().min(1), context: z.string() }),
        ),
        output: json,
        run: (input, context) =>
          authorized(context, async () =>
            toJson(
              await context
                .get(wiring.decisions)
                .request({
                  id: input.id,
                  input: { question: input.question, context: input.context },
                }),
            ),
          ),
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
