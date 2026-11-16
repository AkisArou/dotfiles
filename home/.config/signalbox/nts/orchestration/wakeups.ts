// Wake-ups are events, never a timer. Each observed change that the coordinator
// acts on becomes one mailbox message with a stable identity, so a restart
// re-derives the same messages instead of inventing new ones. The cursor of
// already-announced events lives in the host's state.
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { hash, poll } from "signalbox/core/utils";
import { textInput } from "signalbox/ext/agents/index";
import type { Assignment, Assignments, WorkflowStore } from "signalbox/ext/assignments/index";
import type { Mailbox } from "signalbox/ext/orchestration/services";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { current } from "./current.ts";
import { taskState } from "./task.ts";
import type { TaskState } from "./task.ts";
import type { UserDecisions } from "./tools.ts";

export type Wakeup = { readonly id: string; readonly text: string };

/** How long a phase may take before the coordinator hears of it, once per phase entry. */
export const PHASE_BUDGET: Partial<Record<TaskState["phase"], number>> = {
  design: 2 * 3_600_000,
  approve: 3_600_000,
  build: 8 * 3_600_000,
  reviewing: 2 * 3_600_000,
  queue: 30 * 60_000,
  landing: 3 * 3_600_000,
  verify: 12 * 3_600_000,
};

/** The events in one observation. Pure, so it can be tested on recorded statuses. */
export function wakeups(observed: {
  readonly assignments: readonly Assignment[];
  readonly landing: LandingStatus | undefined;
  readonly fullCheck: FullCheckStatus | undefined;
  readonly decided: readonly { id: string; answer: string }[];
  /** The observation's time, for phase budgets. */
  readonly now: number;
}): Wakeup[] {
  const events: Wakeup[] = [];
  for (const assignment of observed.assignments) {
    if (assignment.route !== "nts/task") continue;
    const state = taskState.safeParse(assignment.state);
    if (!state.success) continue;
    const task = state.data;
    const name = `${assignment.key} (${assignment.id})`;
    if (task.phase === "approve" && task.design)
      events.push({
        id: `design/${assignment.id}/${hash(task.design).slice(0, 16)}`,
        text: `Design ready for ${name}. Read it with list_assignments and answer with decide_design:\n${task.design}`,
      });
    if (task.help)
      events.push({
        id: `help/${assignment.id}/${hash(task.help).slice(0, 16)}`,
        text: `The worker of ${name} asks: ${task.help}\nAnswer with message_worker.`,
      });
    if (task.stalled && task.watch)
      events.push({
        id: `stalled/${assignment.id}/${task.watch.key}`,
        text: `${name} is stalled: ${task.stalled}`,
      });
    const budget = PHASE_BUDGET[task.phase];
    if (assignment.status === "active" && budget && task.phaseSince !== null && observed.now - task.phaseSince > budget)
      events.push({
        id: `overdue/${assignment.id}/${task.phase}/${task.phaseSince}`,
        text:
          `${name} has been in ${task.phase} for ${Math.round((observed.now - task.phaseSince) / 60_000)} min (budget ${budget / 60_000}).` +
          ` Now: ${assignment.wait?.reason ?? "working"}${task.progress ? `; progress: ${task.progress.note}` : ""}. Decide whether it needs you.`,
      });
    if (assignment.wait?.reason.startsWith("Escalated"))
      events.push({
        id: `escalated/${assignment.id}/${task.rounds}`,
        text: `${name} escalated: ${assignment.wait.reason}. Decide: guide the worker, cancel, or ask the user.`,
      });
    if (assignment.error)
      events.push({
        id: `fault/${assignment.id}/${assignment.error.code}/${hash(assignment.error.message).slice(0, 12)}`,
        text: `${name} faulted (${assignment.error.code}): ${assignment.error.message}`,
      });
    if (assignment.status === "finished")
      events.push({ id: `finished/${assignment.id}`, text: `${name} finished: ${task.outcome ?? "done"}.` });
  }
  for (const answer of observed.landing?.recent ?? [])
    if (answer.state !== "landed" || answer.issue !== null)
      events.push({
        id: `landing/${answer.id}/${answer.state}`,
        text: `Landing ${answer.state}: ${answer.issue ? `#${answer.issue} ` : ""}${answer.sha.slice(0, 12)}: ${answer.message}${answer.run ? `\nLogs: ${answer.run}` : ""}`,
      });
  for (const culprit of observed.fullCheck?.culprits ?? [])
    events.push({
      id: `culprit/${culprit.commit}/${culprit.part}/${culprit.revert ?? "open"}`,
      text: `Full check ${culprit.part} red: culprit ${culprit.commit.slice(0, 12)} (${culprit.steps.join(", ")})${culprit.revert ? `, reverted as ${culprit.revert.slice(0, 12)}` : ""}. Logs: ${culprit.run}`,
    });
  for (const decision of observed.decided)
    events.push({ id: `decided/${decision.id}`, text: `The user answered ${decision.id}: ${decision.answer}` });
  return events;
}

export function wakeupFeed(options: {
  readonly state: Capability<WorkflowStore>;
  readonly mailbox: Capability<Mailbox>;
  readonly work: Capability<Assignments>;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
  readonly decisions: Capability<UserDecisions>;
}): Extension {
  const NAMESPACE = "nts/wakeups";
  return {
    id: "nts-wakeups",
    requires: [options.state, options.mailbox, options.work, options.landing, options.fullCheck, options.decisions],
    start(context) {
      const store = context.get(options.state);
      const observe = async () => {
        const events = wakeups({
          assignments: current(context.get(options.work).assignments) ?? [],
          landing: current(context.get(options.landing)),
          fullCheck: current(context.get(options.fullCheck)),
          decided: (current(context.get(options.decisions).requests) ?? [])
            .filter((request) => request.status === "decided" && request.result)
            .map((request) => ({ id: request.id, answer: request.result!.decision.answer })),
          now: Date.now(),
        });
        // The first observation of a fresh state seeds the cursor: history before
        // the coordinator existed is not news.
        const fresh = await store.transaction(NAMESPACE, (tx) => {
          if (tx.get("seeded") !== undefined) return false;
          for (const event of events) tx.set(`seen/${event.id}`, true);
          tx.set("seeded", true);
          return true;
        });
        if (fresh) return;
        const unseen = await store.transaction(NAMESPACE, (tx) =>
          events.filter((event) => tx.get(`seen/${event.id}`) === undefined),
        );
        const mailbox = context.get(options.mailbox);
        for (const event of unseen) {
          await mailbox.send({ id: event.id, input: textInput(event.text) });
          await store.transaction(NAMESPACE, (tx) => tx.set(`seen/${event.id}`, true as Json));
        }
      };
      context.afterStart(() => {
        const loop = poll(observe, 5_000, context.signal, (error) => context.log.error("wake-ups", error));
        context.onStop(() => loop);
      });
    },
  };
}
