// Wake-ups are events, never a timer. Each observed change that the coordinator
// acts on becomes one mailbox message with a stable identity, so a restart
// re-derives the same messages instead of inventing new ones. The cursor of
// already-announced events lives in the host's state.
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { hash, poll } from "signalbox/core/utils";
import { textInput } from "signalbox/ext/agents";
import type { Assignment, Assignments, WorkflowStore } from "signalbox/ext/assignments";
import type { Mailbox } from "signalbox/ext/orchestration";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { latestValue } from "signalbox/core";
import { taskState } from "./task-model.ts";
import type { TaskState } from "./task-model.ts";
import type { Questions } from "./needs.ts";
import type { NeedsView } from "@signalbox/kit/shared/protocol";

export type Wakeup = { readonly id: string; readonly text: string };

export const SOURCES = ["assignments", "landing", "fullCheck", "decisions", "needs"] as const;
/** Sources whose standing state is news: an issue that waited unasked before the coordinator started still needs its question. */
const ANNOUNCES_HISTORY: ReadonlySet<WakeupSource> = new Set(["needs"]);
export type WakeupSource = (typeof SOURCES)[number];
/** Which source an event comes from, by its stable ID. */
export function sourceOf(id: string): WakeupSource {
  if (id.startsWith("landing/")) return "landing";
  if (id.startsWith("culprit/")) return "fullCheck";
  if (id.startsWith("decided/") || id.startsWith("withdrawn/")) return "decisions";
  if (id.startsWith("needs-question/")) return "needs";
  return "assignments";
}

/** How far past its own progress estimate an agent may run before the coordinator hears of it. */
export const PROGRESS_GRACE_MS = 30 * 60_000;

/** How long a phase may take before the coordinator hears of it, once per phase entry. */
export const PHASE_BUDGET: Partial<Record<TaskState["phase"], number>> = {
  design: 2 * 3_600_000,
  approve: 3_600_000,
  signoff: 3_600_000,
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
  readonly decided: readonly {
    id: string;
    question: string;
    issue?: number | undefined;
    answer: string;
    then?: string | undefined;
  }[];
  /** Questions withdrawn (by the owner, or by the coordinator replacing one). */
  readonly withdrawn?: readonly { id: string; question: string; issue?: number | undefined }[];
  /** Issues that wait on the owner with no question asked about them. */
  readonly unasked?: readonly { number: number; title: string }[];
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
    // An agent's own estimate long past: whatever it reads as (a hung background
    // task keeps a Claude session "busy"), the coordinator should look.
    if (
      assignment.status === "active" &&
      task.progress &&
      observed.now > task.progress.until + PROGRESS_GRACE_MS
    )
      events.push({
        id: `overrun/${assignment.id}/${task.progress.at}`,
        text:
          `${name}: its agent said "${task.progress.note}" and expected to be done ${Math.round((observed.now - task.progress.until) / 60_000)} min ago.` +
          " Read its terminal (a hung background command keeps it busy), then message it or cancel.",
      });
    const budget = PHASE_BUDGET[task.phase];
    if (
      assignment.status === "active" &&
      budget &&
      task.phaseSince !== null &&
      observed.now - task.phaseSince > budget
    )
      events.push({
        id: `overdue/${assignment.id}/${task.phase}/${task.phaseSince}`,
        text:
          `${name} has been in ${task.phase} for ${Math.round((observed.now - task.phaseSince) / 60_000)} min (budget ${budget / 60_000}).` +
          ` Now: ${assignment.wait?.reason ?? "working"}${task.progress ? `; progress: ${task.progress.note}` : ""}. Decide whether it needs you.`,
      });
    if (task.phase === "signoff" && task.commit)
      events.push({
        id: `signoff/${assignment.id}/${task.commit}`,
        text:
          `${name}: the reviewed commit ${task.commit} changes core paths (${task.core.join(", ")}); AGENTS.md has you read it before it is submitted.` +
          ` Read the diff in ${task.tree} (git show ${task.commit.slice(0, 12)}) and its proof, then sign_off: accept to queue it, or refuse with what must change.`,
      });
    if (task.phase === "escalated" && task.escalation)
      events.push({
        id: `escalated/${assignment.id}/${hash(task.escalation).slice(0, 16)}`,
        text: `${name} needs your decision: ${task.escalation}\nDecide with message_worker (it returns to build with your message), cancel_assignment, or ask_user.`,
      });
    if (assignment.error)
      events.push({
        id: `fault/${assignment.id}/${assignment.error.code}/${hash(assignment.error.message).slice(0, 12)}`,
        text: `${name} faulted (${assignment.error.code}): ${assignment.error.message}`,
      });
    if (assignment.status === "finished")
      events.push({
        id: `finished/${assignment.id}`,
        text: `${name} finished: ${task.outcome ?? "done"}.`,
      });
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
    events.push({
      id: `decided/${decision.id}`,
      text:
        `The owner answered ${decision.id}${decision.issue ? ` (about #${decision.issue})` : ""}: "${decision.question}"\n` +
        `Answer: ${decision.answer}` +
        (decision.issue && decision.then
          ? `\n#${decision.issue} is now ${decision.then}; the answer is posted on it.`
          : "") +
        "\nAct on it.",
    });
  for (const question of observed.withdrawn ?? [])
    events.push({
      id: `withdrawn/${question.id}`,
      text:
        `The question ${question.id} ("${question.question}") was withdrawn. ` +
        "If it still needs the owner, ask again with ask_user: name the issue it is about and give options, each with then.",
    });
  for (const issue of observed.unasked ?? [])
    events.push({
      id: `needs-question/${issue.number}`,
      text:
        `#${issue.number} ${issue.title} waits on the owner (state:needs-user), but nobody has asked them a question. ` +
        `Read it, then ask_user with issue ${issue.number}: the decision in one sentence, what they need to decide it in context, ` +
        "and options when the answer is a choice, each with then (ready, parked or held).",
    });
  return events;
}

export function wakeupFeed(options: {
  readonly state: Capability<WorkflowStore>;
  readonly mailbox: Capability<Mailbox>;
  readonly work: Capability<Assignments>;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
  readonly decisions: Capability<Questions>;
  readonly needs: Capability<Readable<NeedsView>>;
}): Extension {
  const NAMESPACE = "nts/wakeups";
  return {
    id: "nts-wakeups",
    requires: [
      options.state,
      options.mailbox,
      options.work,
      options.landing,
      options.fullCheck,
      options.decisions,
      options.needs,
    ],
    start(context) {
      const store = context.get(options.state);
      const observe = async () => {
        const events = wakeups({
          assignments: latestValue(context.get(options.work).assignments) ?? [],
          landing: latestValue(context.get(options.landing)),
          fullCheck: latestValue(context.get(options.fullCheck)),
          decided: (latestValue(context.get(options.decisions).requests) ?? [])
            .filter((request) => request.status === "decided" && request.result)
            .map((request) => ({
              id: request.id,
              question: request.input.question,
              issue: request.input.issue,
              answer: request.result!.decision.answer,
              then: request.result!.decision.then,
            })),
          withdrawn: (latestValue(context.get(options.decisions).requests) ?? [])
            .filter((request) => request.status === "withdrawn")
            .map((request) => ({
              id: request.id,
              question: request.input.question,
              issue: request.input.issue,
            })),
          unasked: latestValue(context.get(options.needs))?.unasked ?? [],
          now: Date.now(),
        });
        // Each source's history before the coordinator saw it is not news: its
        // events are marked seen the first time that source reads ready, and it
        // says nothing until then. (State from before per-source seeding has one
        // "seeded" mark that covers every source.)
        const readings: Record<WakeupSource, { status: string }> = {
          assignments: context.get(options.work).assignments.get(),
          landing: context.get(options.landing).get(),
          fullCheck: context.get(options.fullCheck).get(),
          decisions: context.get(options.decisions).requests.get(),
          needs: context.get(options.needs).get(),
        };
        const unseen = await store.transaction(NAMESPACE, (tx) => {
          const legacy = tx.get("seeded") !== undefined;
          const seeded = (source: WakeupSource) =>
            legacy || tx.get(`seeded/${source}`) !== undefined;
          for (const source of SOURCES) {
            if (seeded(source) || readings[source].status !== "ready") continue;
            if (ANNOUNCES_HISTORY.has(source)) {
              tx.set(`seeded/${source}`, true);
              continue;
            }
            for (const event of events)
              if (sourceOf(event.id) === source) tx.set(`seen/${event.id}`, true);
            tx.set(`seeded/${source}`, true);
          }
          return events.filter(
            (event) => seeded(sourceOf(event.id)) && tx.get(`seen/${event.id}`) === undefined,
          );
        });
        const mailbox = context.get(options.mailbox);
        for (const event of unseen) {
          await mailbox.send({ id: event.id, input: textInput(event.text) });
          await store.transaction(NAMESPACE, (tx) => tx.set(`seen/${event.id}`, true as Json));
        }
      };
      context.afterStart(() => {
        const loop = poll(observe, 5_000, context.signal, (error) =>
          context.log.error("wake-ups", error),
        );
        context.onStop(() => loop);
      });
    },
  };
}
