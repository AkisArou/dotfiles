// What the NTS task does in each phase. Every external action is a durable
// effect recovered from NTS's own state (the pool, the landing queue), never
// guessed; effect keys are permanent, so a live task resumes across versions.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import type { Capability, Readable } from "signalbox/core";
import { latestValue } from "signalbox/core";
import { Conflict, hash } from "signalbox/core/utils";
import { textInput } from "signalbox/ext/agents/index";
import type { AgentRuntime } from "signalbox/ext/agents/index";
import { messaging, terminate } from "signalbox/ext/agents/features";
import type { AgentToolset } from "signalbox/ext/agents/tools";
import type { AssignmentContext, AssignmentStep } from "signalbox/ext/assignments/index";
import type { Issue } from "../shared/model.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { landingFor, landingStatus, poolStatus, runNts, treeForIssue } from "../status/commands.ts";
import type { ManagedAgents } from "./agents.ts";
import type { Profiles } from "./profiles.ts";
import {
  reviewerBrief,
  reviewerInstructions,
  workerBrief,
  workerInstructions,
  workerResumeBrief,
} from "./briefs.ts";
import { progressed, STALL, stallStep } from "./stall.ts";
import { awaitedAgent, cardPaths } from "./task-model.ts";
import type { Phase, TaskInput, TaskReport, TaskReportOf, TaskState } from "./task-model.ts";

const run = promisify(execFile);

export interface TaskOptions {
  readonly root: string;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
  readonly profiles: Capability<Profiles>;
  readonly agents: ManagedAgents;
  readonly workerTools: AgentToolset;
  readonly reviewerTools: AgentToolset;
  /** Returns to build before the task escalates instead of looping. */
  readonly maxRounds: number;
  /** The clock for phase times, progress and stalls; tests advance their own. */
  readonly clock: () => number;
}

type Step = AssignmentStep<TaskState>;
export type TaskContext = AssignmentContext<TaskInput, TaskState, TaskReport>;

/** One reconcile of one task: its state, and the actions every phase shares. */
export interface Turn {
  readonly ctx: TaskContext;
  readonly state: TaskState;
  readonly options: TaskOptions;
  readonly runtime: AgentRuntime;
  /** The first unconsumed report of a kind (that matches), consumed with the next commit. */
  take<K extends TaskReport["kind"]>(
    kind: K,
    match?: (report: TaskReportOf<K>) => boolean,
  ): (TaskReportOf<K> & { readonly id: string }) | undefined;
  next(patch: Partial<TaskState>): Step;
  wait(reason: string, patch?: Partial<TaskState>, retryAt?: number): Step;
  /** The task's card, while the issues source lists it. */
  issue(): Issue | undefined;
  /** Provably ended: an unknown session state is not an end. */
  ended(id: string): boolean;
  tell(to: string, key: string, text: string): Promise<void>;
  stop(id: string, key: string): Promise<void>;
}

export function turn(ctx: TaskContext, options: TaskOptions): Turn {
  const state = ctx.assignment.state;
  const runtime = ctx.get(options.agents.runtime);
  // A new phase restarts its clock and forgets the previous phase's watch.
  const moved = (patch: Partial<TaskState>): TaskState =>
    patch.phase !== undefined && patch.phase !== state.phase
      ? {
          ...state,
          phaseSince: options.clock(),
          watch: null,
          progress: null,
          stalled: null,
          ...patch,
        }
      : { ...state, ...patch };
  const ended = (id: string) =>
    (latestValue(runtime.instances) ?? []).some(
      (instance) =>
        instance.id === id && (instance.state === "ended" || instance.state === "exited"),
    );
  return {
    ctx,
    state,
    options,
    runtime,
    take(kind, match) {
      const found = ctx.reports.find(
        (report) => report.data.kind === kind && (!match || match(report.data as never)),
      );
      if (!found) return undefined;
      ctx.consumeReports(found.id);
      return { ...(found.data as TaskReportOf<typeof kind>), id: found.id } as never;
    },
    next: (patch) => ({ state: moved(patch), next: "continue" }),
    wait: (reason, patch = {}, retryAt) => ({
      state: moved(patch),
      next: "wait",
      reason,
      ...(retryAt === undefined ? {} : { retryAt }),
    }),
    issue: () =>
      latestValue(ctx.get(options.issues))?.find(
        (candidate) => candidate.number === ctx.input.issue,
      ),
    ended,
    async tell(to, key, text) {
      if (ended(to)) return;
      const send = runtime.capability(to, messaging);
      if (!send) throw new Error(`Agent ${to} cannot receive messages`);
      await ctx.effect({
        key: `tell/${key}`,
        input: { to, text },
        run: async ({ requestId }) => {
          await send.submit(textInput(text), { requestId, signal: ctx.signal });
          return null;
        },
      });
    },
    async stop(id, key) {
      const end = runtime.capability(id, terminate);
      if (!end) return;
      await ctx.effect({
        key: `stop/${key}`,
        input: { id },
        idempotent: true,
        run: async () => {
          await end.terminate(ctx.signal);
          return null;
        },
      });
    },
  };
}

const firstLine = (text: string) => text.split("\n", 1)[0]!;

/** Hand the task to the coordinator; nothing moves until it decides. */
const escalate = (t: Turn, escalation: string, patch: Partial<TaskState>) =>
  t.next({ phase: "escalated", escalation, ...patch });

/** Findings send the change back to build, until the rounds run out and the coordinator decides. */
async function returnToBuild(
  t: Turn,
  findings: { readonly key: string; readonly note: string; readonly why: string },
  patch: Partial<TaskState> = {},
): Promise<Step> {
  const rounds = t.state.rounds + 1;
  if (rounds > t.options.maxRounds)
    return escalate(t, `Escalated after ${rounds} rounds: ${findings.why}`, {
      rounds,
      note: findings.note,
      ...patch,
    });
  await t.tell(t.state.worker!.id, findings.key, findings.note);
  return t.next({ phase: "build", rounds, note: findings.note, ...patch });
}

/** Waiting on an agent's report: an idle agent is nudged, then the coordinator is told (stall.ts). */
async function awaiting(t: Turn, reason: string, ask: string): Promise<Step> {
  const awaited = awaitedAgent(t.state);
  if (!awaited) return t.wait(reason);
  const now = t.options.clock();
  const step = stallStep({
    key: awaited.key,
    status: latestValue(t.ctx.get(t.options.agents.activity))?.[awaited.id],
    watch: t.state.watch,
    now,
  });
  if (step.action === "nudge")
    await t.tell(
      awaited.id,
      // One nudge per idle stretch: its start is persisted, so a retried reconcile reuses it.
      `nudge/${awaited.key}/${t.state.watch?.idleSince ?? now}`,
      `Task #${t.ctx.input.issue} waits for ${ask}. You have been idle ${Math.round(STALL.idle / 60_000)} minutes. ` +
        `If it is done, call that tool now. If you are waiting for background work, call report_progress with what you wait for and the minutes it needs. ` +
        (awaited.role === "worker"
          ? "If something blocks you, call ask_coordinator."
          : "If something blocks the review, say so in report_review."),
    );
  const stalled =
    step.action === "escalate"
      ? `The ${awaited.role} (${awaited.id}) has stayed idle without ${ask} through ${STALL.nudges} nudges` +
        (t.state.progress ? `; its last progress: ${t.state.progress.note}` : "") +
        ". Look at its terminal, message it, or cancel the task."
      : t.state.stalled;
  return t.wait(reason, { watch: step.watch, stalled }, step.retryAt);
}

type Interrupt = (t: Turn) => Promise<Step | undefined> | Step | undefined;

/** Handled before the phase, in this order, whatever the phase is. */
export const interrupts: readonly Interrupt[] = [
  // Progress: the awaited agent's own estimate holds nudges until then.
  (t) => {
    const report = t.take("progress");
    if (!report) return undefined;
    const awaited = awaitedAgent(t.state);
    const now = t.options.clock();
    const until = now + report.minutes * 60_000;
    return t.next({
      progress: { note: report.note, at: now, until },
      ...(awaited ? { watch: progressed(awaited.key, until), stalled: null } : {}),
    });
  },
  // A message reaches the worker once; it answers open help and decides an escalation.
  async (t) => {
    const report = t.take("message");
    if (!report) return undefined;
    if (!t.state.worker) throw new Conflict("This task has no current worker to message");
    const text = `From the ${report.from}: ${report.text}`;
    if (t.state.phase !== "escalated") {
      await t.tell(t.state.worker.id, `message/${report.id}`, text);
      return t.next({ help: null, stalled: null, note: text });
    }
    const decided = `${text}\n\nIt decides on:\n${t.state.note}`;
    await t.tell(t.state.worker.id, `message/${report.id}`, decided);
    return t.next({ phase: "build", escalation: null, help: null, note: decided });
  },
  // Help is answered by the coordinator; it never blocks a report that follows it.
  (t) => {
    const report = t.take("help");
    return report && t.next({ help: report.message });
  },
  // Rework from landing: withdraw the change's own queue entry (a held one
  // never lands by itself), tell the worker why, and build again.
  async (t) => {
    if (t.state.phase !== "landing" && t.state.phase !== "queue") return undefined;
    const report = t.take("rework");
    if (!report) return undefined;
    const commit = t.state.commit!;
    await t.ctx.effect({
      key: `withdraw/${commit}/${report.id}`,
      input: { commit },
      idempotent: true,
      async run({ signal }) {
        const found = landingFor(await landingStatus(t.options.root, signal), commit);
        if (!found.checking && !found.waiting && !found.held) return null;
        const withdrawn = await runNts(
          t.options.root,
          ["tooling/landing/check.ts", "withdraw", commit],
          {
            signal,
            timeout: 120_000,
          },
        );
        if (withdrawn.code !== 0)
          throw new Error(
            `check.ts withdraw ${commit} exited ${withdrawn.code}: ${(withdrawn.stderr || withdrawn.stdout).trim()}`,
          );
        return null;
      },
    });
    const note = `Rework ${commit}: ${report.notes}\nFix the cause where it lives, prove the change again, commit and report_ready.`;
    await t.tell(t.state.worker!.id, `rework/${report.id}`, note);
    return t.next({ phase: "build", rounds: t.state.rounds + 1, note, help: null });
  },
  // A worker whose session provably ended is replaced, resuming where it stood.
  (t) =>
    t.state.worker &&
    (t.state.phase === "design" || t.state.phase === "approve" || t.state.phase === "build") &&
    t.ended(t.state.worker.id)
      ? t.next({ phase: "start", resumeTo: t.state.phase })
      : undefined,
];

type PhaseHandler = (t: Turn) => Promise<Step> | Step;

export const phaseHandlers: Readonly<Record<Phase, PhaseHandler>> = {
  prepare(t) {
    const found = t.issue();
    if (!found) return t.wait(`Issue #${t.ctx.input.issue} is not available`);
    return t.next({
      phase: "workspace",
      title: found.title,
      revision: hash({ body: found.body, labels: found.labels }),
      paths: cardPaths(found.body),
    });
  },

  async workspace(t) {
    const { root } = t.options;
    const issue = t.ctx.input.issue;
    const tree = await t.ctx.effect<{ path: string; branch: string }>({
      key: "take-tree",
      input: { issue },
      async recover({ signal }) {
        const held = treeForIssue(await poolStatus(root, signal), issue);
        return held
          ? { status: "completed" as const, value: { path: held.path, branch: held.task! } }
          : { status: "absent" as const };
      },
      async run({ signal }) {
        const taken = await runNts(
          root,
          ["tooling/landing/pool.ts", "take", "--issue", String(issue)],
          {
            signal,
            timeout: 600_000,
          },
        );
        const held = treeForIssue(await poolStatus(root, signal), issue);
        if (!held)
          throw new Error(
            `pool.ts take exited ${taken.code}: ${(taken.stderr || taken.stdout).trim()}`,
          );
        return { path: held.path, branch: held.task! };
      },
    });
    return t.next({ phase: "start", tree: tree.path, branch: tree.branch });
  },

  async start(t) {
    const { state, ctx, options } = t;
    const resolved = await ctx.get(options.profiles).resolve("worker");
    if ("wait" in resolved) return t.wait(resolved.wait, {}, resolved.retryAt);
    const found = t.issue();
    if (!found) return t.wait(`Issue #${ctx.input.issue} is not available`);
    const generation = state.generation + 1;
    const card = {
      assignment: ctx.assignment.id,
      issue: found,
      branch: state.branch!,
      tree: state.tree!,
    };
    const brief = state.resumeTo
      ? workerResumeBrief({
          ...card,
          phase: state.resumeTo,
          design: state.design,
          commit: state.commit,
          note: state.note,
        })
      : workerBrief({ ...card, instructions: ctx.input.instructions });
    const started = await ctx.effect<{ id: string }>({
      // The first launch keeps its original key, so receipts from before generations recover.
      key: generation === 1 ? "start-worker" : `start-worker/${generation}`,
      input: { tree: state.tree, profile: resolved.profile, generation },
      idempotent: true,
      run: async ({ requestId, signal }) => ({
        id: (
          await options.agents.start(
            t.runtime,
            {
              profile: resolved.profile,
              label: `#${ctx.input.issue} worker`,
              cwd: state.tree!,
              instructions: workerInstructions(),
              brief,
              tools: [options.workerTools],
              requestId,
            },
            signal,
          )
        ).id,
      }),
    });
    return t.next({
      phase: state.resumeTo ?? "design",
      worker: { id: started.id, profile: resolved.profile },
      generation,
      resumeTo: null,
    });
  },

  design(t) {
    const report = t.take("design");
    if (!report)
      return awaiting(t, "Waiting for the worker's design", "your design (report_design)");
    return t.next({ phase: "approve", design: report.summary, help: null });
  },

  async approve(t) {
    const report = t.take("design_decision");
    if (!report) return t.wait("Waiting for the coordinator to approve the design");
    const decision = report.accept
      ? `Design approved. ${report.notes}\nBuild it, prove it (focused tests, inner.ts, check.ts --here), commit, then report_ready.`
      : `Design returned. ${report.notes}\nRevise it and report_design again.`;
    await t.tell(t.state.worker!.id, `design/${report.id}`, decision);
    return t.next({ phase: report.accept ? "build" : "design", note: decision });
  },

  async build(t) {
    const report = t.take("ready");
    if (!report)
      return awaiting(
        t,
        "Waiting for the worker's proved commit",
        "your proved commit (report_ready)",
      );
    const git = async (args: string[]) =>
      (
        await run("git", ["--no-optional-locks", ...args], { cwd: t.state.tree!, timeout: 30_000 })
      ).stdout.trim();
    if (
      (await git(["rev-parse", "HEAD"])) !== report.commit ||
      (await git(["status", "--porcelain"]))
    )
      throw new Conflict("The reported commit is not the clean HEAD of the task's tree");
    return t.next({ phase: "review", commit: report.commit, summary: report.summary, help: null });
  },

  async review(t) {
    const { state, ctx, options } = t;
    const resolved = await ctx.get(options.profiles).resolve("reviewer");
    if ("wait" in resolved) return t.wait(resolved.wait, {}, resolved.retryAt);
    const commit = state.commit!;
    const brief = reviewerBrief({
      assignment: ctx.assignment.id,
      issue: ctx.input.issue,
      title: state.title,
      commit,
      tree: state.tree!,
      design: state.design,
      summary: state.summary,
      previous: state.reviews,
      labels: t.issue()?.labels ?? [],
    });
    const started = await ctx.effect<{ id: string }>({
      // Each review round gets a fresh reviewer, even of the same commit.
      key: `start-reviewer/${commit}/${state.rounds}`,
      input: { tree: state.tree, commit, round: state.rounds, profile: resolved.profile },
      idempotent: true,
      run: async ({ requestId, signal }) => ({
        id: (
          await options.agents.start(
            t.runtime,
            {
              profile: resolved.profile,
              label: `#${ctx.input.issue} review ${commit.slice(0, 12)}`,
              cwd: state.tree!,
              instructions: reviewerInstructions(),
              brief,
              tools: [options.reviewerTools],
              requestId,
            },
            signal,
          )
        ).id,
      }),
    });
    return t.next({
      phase: "reviewing",
      reviewer: { id: started.id, commit, round: state.rounds, profile: resolved.profile },
    });
  },

  async reviewing(t) {
    const { state } = t;
    const report = t.take("review", (review) => review.commit === state.commit);
    if (!report)
      return awaiting(
        t,
        "Waiting for the review of the exact commit",
        `your review of ${state.commit} (report_review)`,
      );
    await t.stop(state.reviewer!.id, `reviewer/${state.reviewer!.commit}/${state.reviewer!.round}`);
    if (report.verdict === "accept") return t.next({ phase: "queue", reviewer: null });
    const reviews = [...state.reviews, { commit: state.commit!, findings: report.findings }];
    // A dispute of the approved design is the coordinator's call, not a round
    // of rework the worker starts on its own.
    if (report.disputesDesign)
      return escalate(
        t,
        `The reviewer of ${state.commit} disputes the approved design:\n${report.findings}\nDecide: message the worker to change course, or to keep the design and answer the finding.`,
        {
          reviewer: null,
          reviews,
          // A review round ended: the next review of this commit is a fresh reviewer.
          rounds: state.rounds + 1,
          note: `The review of ${state.commit} disputes the approved design:\n${report.findings}`,
        },
      );
    return returnToBuild(
      t,
      {
        key: `review/${report.id}`,
        note: `The review of ${state.commit} asks for changes:\n${report.findings}\nFix them, prove the change again, commit and report_ready.`,
        why: `the review of ${state.commit} asks for changes again`,
      },
      { reviewer: null, reviews },
    );
  },

  escalated: (t) =>
    t.wait(`The coordinator decides: ${firstLine(t.state.escalation ?? "escalated")}`),

  async queue(t) {
    const { state, options } = t;
    const commit = state.commit!;
    // One submission per commit and round: a reworked commit queues again.
    const queuedAt = await t.ctx.effect<number>({
      key: state.rounds === 0 ? `queue/${commit}` : `queue/${commit}/${state.rounds}`,
      input: { commit, round: state.rounds },
      async recover({ signal }) {
        const found = landingFor(await landingStatus(options.root, signal), commit);
        return found.checking || found.waiting || found.held
          ? { status: "completed" as const, value: options.clock() }
          : { status: "absent" as const };
      },
      async run({ signal }) {
        const queued = await runNts(
          state.tree!,
          ["tooling/landing/check.ts", commit, "--no-wait"],
          {
            signal,
            timeout: 300_000,
          },
        );
        if (queued.code !== 0)
          throw new Error(
            `check.ts ${commit} exited ${queued.code}: ${(queued.stderr || queued.stdout).trim()}`,
          );
        return options.clock();
      },
    });
    return t.next({ phase: "landing", queuedAt });
  },

  landing(t) {
    const { state } = t;
    const status = latestValue(t.ctx.get(t.options.landing));
    if (!status) return t.wait("Waiting for the landing status");
    const answer = landingFor(status, state.commit!).answers.find(
      // The landing service and this host share the machine's clock.
      (candidate) => state.queuedAt === null || Date.parse(candidate.at) >= state.queuedAt,
    );
    if (!answer) return t.wait("Waiting for the landing check's answer");
    if (answer.state === "landed")
      return t.next({ phase: "verify", landedMain: status.main.commit ?? state.commit });
    if (answer.state === "held") return t.wait(`Landing held: ${answer.message}`);
    if (answer.state === "withdrawn") return t.next({ phase: "retire", outcome: "withdrawn" });
    return returnToBuild(t, {
      key: `refused/${answer.id}`,
      note: `The landing check refused ${state.commit}: ${answer.message}\nLogs: ${answer.run ?? "none"}\nFind the cause, fix it where it lives, prove it again, commit and report_ready.`,
      why: `the landing check refused ${state.commit}`,
    });
  },

  async verify(t) {
    const verified = latestValue(t.ctx.get(t.options.fullCheck))?.verified.commit;
    if (!verified) return t.wait("Waiting for the full check to verify the landing");
    const contained = await run(
      "git",
      ["--no-optional-locks", "merge-base", "--is-ancestor", t.state.landedMain!, verified],
      { cwd: t.options.root, timeout: 30_000 },
    ).then(
      () => true,
      () => false,
    );
    if (!contained)
      return t.wait(`Waiting for verified to reach ${t.state.landedMain!.slice(0, 12)}`);
    return t.next({ phase: "retire", outcome: "verified" });
  },

  async retire(t) {
    const { state } = t;
    if (state.worker) await t.stop(state.worker.id, "worker");
    if (state.reviewer)
      await t.stop(state.reviewer.id, `reviewer/${state.reviewer.commit}/${state.reviewer.round}`);
    return { state: { ...state, phase: "done" }, next: "finish" };
  },

  done: (t) => ({ state: t.state, next: "finish" }),
};

/** Cancelling ends this task's sessions; its tree and branch stay with NTS's pool. */
export async function cancelTask(
  ctx: TaskContext & { readonly reason: string },
  options: TaskOptions,
): Promise<Step> {
  const runtime = ctx.get(options.agents.runtime);
  for (const agent of [ctx.assignment.state.worker, ctx.assignment.state.reviewer]) {
    const end = agent && runtime.capability(agent.id, terminate);
    if (!agent || !end) continue;
    await ctx.effect({
      key: `cancel-stop/${agent.id}`,
      input: { id: agent.id },
      idempotent: true,
      run: async () => {
        await end.terminate(ctx.signal);
        return null;
      },
    });
  }
  return {
    state: { ...ctx.assignment.state, phase: "done", outcome: `cancelled: ${ctx.reason}` },
    next: "finish",
  };
}
