// The NTS task route: one GitHub issue, one fresh worker, NTS's own gates.
// Phases follow docs/workflow.md; every external action is a durable effect
// recovered from NTS's own state (the pool, the landing queue), never guessed.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { z } from "zod";
import type { Capability, Readable } from "signalbox/core";
import { current } from "./current.ts";
import { Conflict, hash } from "signalbox/core/utils";
import { textInput } from "signalbox/ext/agents/index";
import { messaging, terminate } from "signalbox/ext/agents/features";
import type { AgentToolset } from "signalbox/ext/agents/tools";
import { assignmentRoute } from "signalbox/ext/assignments/index";
import type { Issue } from "../shared/model.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { landingFor, landingStatus, poolStatus, runNts, treeForIssue } from "../status/commands.ts";
import type { ManagedAgents } from "./agents.ts";
import type { Profiles } from "./profiles.ts";
import { profileSchema } from "./profiles.ts";
import { reviewerBrief, workerBrief, workerInstructions, workerResumeBrief, reviewerInstructions } from "./briefs.ts";
import { progressed, stallStep, STALL, watchState } from "./stall.ts";

const run = promisify(execFile);
const sha = z.string().regex(/^[0-9a-f]{40}$/);

export const taskInput = z.object({
  issue: z.number().int().positive(),
  /** The coordinator's note for this task: scope, order, things to watch. */
  instructions: z.string().default(""),
});
export type TaskInput = z.infer<typeof taskInput>;

export const taskReport = z.discriminatedUnion("kind", [
  z.object({ kind: z.literal("design"), summary: z.string().min(1) }),
  z.object({ kind: z.literal("design_decision"), accept: z.boolean(), notes: z.string() }),
  z.object({ kind: z.literal("ready"), commit: sha, summary: z.string().min(1) }),
  z.object({
    kind: z.literal("review"),
    commit: sha,
    verdict: z.enum(["accept", "changes"]),
    findings: z.string(),
    /** The reviewer disputes the approved design itself; the coordinator decides. */
    disputesDesign: z.boolean().default(false),
  }),
  z.object({ kind: z.literal("help"), message: z.string().min(1) }),
  /** The coordinator (or user) sends a held or landed-but-wrong change back to build. */
  z.object({ kind: z.literal("rework"), notes: z.string().min(1) }),
  /** An agent says what it waits for and for how long; idle until then is expected. */
  z.object({
    kind: z.literal("progress"),
    role: z.enum(["worker", "reviewer"]),
    note: z.string().min(1),
    minutes: z.number().int().min(1).max(480),
  }),
  /** The coordinator's message to the worker: delivered once, and it answers any open help. */
  z.object({ kind: z.literal("message"), from: z.enum(["coordinator", "user"]), text: z.string().min(1) }),
]);
export type TaskReport = z.infer<typeof taskReport>;

export const phases = [
  "prepare",
  "workspace",
  "start",
  "design",
  "approve",
  "build",
  "review",
  "reviewing",
  "queue",
  "landing",
  "verify",
  "retire",
  "done",
] as const;
export const taskState = z.object({
  phase: z.enum(phases),
  title: z.string(),
  revision: z.string(),
  paths: z.array(z.string()),
  tree: z.string().nullable(),
  branch: z.string().nullable(),
  worker: z.object({ id: z.string(), profile: profileSchema }).nullable(),
  reviewer: z.object({ id: z.string(), commit: sha, round: z.number().int(), profile: profileSchema }).nullable(),
  design: z.string(),
  commit: sha.nullable(),
  summary: z.string(),
  /** Returns to build: review findings and landing refusals together. */
  rounds: z.number().int().nonnegative(),
  landedMain: sha.nullable(),
  help: z.string().nullable(),
  outcome: z.string().nullable(),
  /** Worker launches so far; a replacement after a proven-ended session is a new generation. */
  // Defaults keep state written before these fields readable; a pre-existing
  // assignment counts as generation 1, so its replacement gets a fresh launch key.
  generation: z.number().int().nonnegative().default(1),
  /** The phase a replacement worker resumes, and the latest instruction it must see. */
  resumeTo: z.enum(phases).nullable().default(null),
  note: z.string().default(""),
  /** Every earlier review round's findings, so each fresh reviewer checks them first. */
  reviews: z.array(z.object({ commit: sha, findings: z.string() })).default([]),
  /** When this round queued its commit; only landing answers after it count. */
  queuedAt: z.number().nullable().default(null),
  /** When the current phase began (ms); the coordinator hears of a phase past its budget. */
  phaseSince: z.number().nullable().default(null),
  /** The awaited agent's idle watch (stall.ts). */
  watch: watchState.nullable().default(null),
  /** The awaited agent's latest progress note. */
  progress: z.object({ note: z.string(), at: z.number(), until: z.number() }).nullable().default(null),
  /** Set when an awaited agent stayed idle through its nudges; the coordinator decides. */
  stalled: z.string().nullable().default(null),
});
export type TaskState = z.infer<typeof taskState>;

/** Paths named on the card's **Paths:** line, as resources two tasks must not share. */
export function cardPaths(body: string): string[] {
  const line = body.split("\n").find((text) => /^\*\*Paths:\*\*/.test(text.trim()));
  if (!line) return [];
  const tokens = line.replace(/^\s*\*\*Paths:\*\*/, "").match(/[\w.@-]+(?:\/[\w.@*{}-]+)+/g) ?? [];
  return [...new Set(tokens.map((token) => token.replace(/[.,;:]+$/, "")))].sort();
}

/** The agent whose report the task waits for, if any. A worker with an open question waits on the coordinator. */
export function awaitedAgent(state: TaskState): { role: "worker" | "reviewer"; id: string; key: string } | undefined {
  if ((state.phase === "design" || state.phase === "build") && state.worker && !state.help)
    return { role: "worker", id: state.worker.id, key: `${state.phase}/${state.worker.id}/${state.rounds}` };
  if (state.phase === "reviewing" && state.reviewer)
    return { role: "reviewer", id: state.reviewer.id, key: `reviewing/${state.reviewer.id}/${state.reviewer.round}` };
  return undefined;
}

export function taskRoute(options: {
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
  /** The route's clock for phase times, progress and stalls; tests advance their own. */
  readonly clock?: () => number;
}) {
  const activity = options.agents.activity;
  const clock = options.clock ?? Date.now;
  const issue = (issues: readonly Issue[], number: number) =>
    issues.find((candidate) => candidate.number === number);
  return assignmentRoute({
    id: "nts/task",
    // Bump with any change to state or phase meaning; old assignments then fault
    // visibly instead of being reinterpreted (keep the old revision until they drain).
    revision: "1",
    input: taskInput,
    concurrency: 4,
    resources: (input, state) => [
      { resource: `issue/${input.issue}`, units: 1 },
      ...(state.phase === "prepare" || state.phase === "done" ? [] : [{ resource: "worker", units: 1 }]),
      ...state.paths.map((path) => ({ resource: `file:${path}`, units: 1 })),
    ],
    admission: [
      {
        requires: [options.issues],
        watch: (ctx) => [ctx.get(options.issues)],
        check(ctx) {
          const issues = current(ctx.get(options.issues));
          if (!issues) return { allowed: false, reason: "Issues are not read yet" };
          const found = issue(issues, ctx.input.issue);
          if (!found) return { allowed: false, reason: `Issue #${ctx.input.issue} is not an open task` };
          const blocked = found.labels.find((label) =>
            ["state:needs-user", "state:held", "state:parked"].includes(label),
          );
          if (blocked) return { allowed: false, reason: `Issue #${ctx.input.issue} is ${blocked}` };
          if (!found.labels.includes("task"))
            return { allowed: false, reason: `Issue #${ctx.input.issue} is not a task issue` };
          return { allowed: true };
        },
      },
    ],
    lifecycle: {
      requires: [options.issues, options.landing, options.fullCheck, options.profiles, options.agents.runtime, activity],
      watch: (ctx) => [
        ctx.get(options.landing),
        ctx.get(options.fullCheck),
        ctx.get(options.agents.runtime).instances,
        ctx.get(activity),
      ],
      state: taskState,
      report: taskReport,
      initial: () => ({
        phase: "prepare" as const,
        title: "",
        revision: "",
        paths: [],
        tree: null,
        branch: null,
        worker: null,
        reviewer: null,
        design: "",
        commit: null,
        summary: "",
        rounds: 0,
        landedMain: null,
        help: null,
        outcome: null,
        generation: 0,
        resumeTo: null,
        note: "",
        reviews: [],
        queuedAt: null,
        phaseSince: null,
        watch: null,
        progress: null,
        stalled: null,
      }),
      async reconcile(ctx) {
        const state = ctx.assignment.state;
        const runtime = ctx.get(options.agents.runtime);
        // A new phase restarts its clock and forgets the previous phase's watch.
        const moved = (patch: Partial<TaskState>): TaskState =>
          patch.phase !== undefined && patch.phase !== state.phase
            ? { ...state, phaseSince: clock(), watch: null, progress: null, stalled: null, ...patch }
            : { ...state, ...patch };
        const next = (patch: Partial<TaskState>) => ({
          state: moved(patch),
          next: "continue" as const,
        });
        const wait = (reason: string, patch: Partial<TaskState> = {}, retryAt?: number) => ({
          state: moved(patch),
          next: "wait" as const,
          reason,
          ...(retryAt === undefined ? {} : { retryAt }),
        });
        // A session is replaced only when it has provably ended; unknown state waits.
        const ended = (id: string) =>
          (current(runtime.instances) ?? []).some(
            (instance) => instance.id === id && (instance.state === "ended" || instance.state === "exited"),
          );
        const tell = async (to: string, key: string, text: string) => {
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
        };
        const stop = async (id: string, key: string) => {
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
        };
        // Progress: the awaited agent's own estimate holds nudges until then.
        const progress = ctx.reports.find((report) => report.data.kind === "progress");
        if (progress && progress.data.kind === "progress") {
          ctx.consumeReports(progress.id);
          const awaited = awaitedAgent(state);
          const now = clock();
          const until = now + progress.data.minutes * 60_000;
          return next({
            progress: { note: progress.data.note, at: now, until },
            ...(awaited ? { watch: progressed(awaited.key, until), stalled: null } : {}),
          });
        }
        // The coordinator's message reaches the worker once and answers its open question.
        const message = ctx.reports.find((report) => report.data.kind === "message");
        if (message && message.data.kind === "message") {
          ctx.consumeReports(message.id);
          if (!state.worker) throw new Conflict("This task has no current worker to message");
          const text = `From the ${message.data.from}: ${message.data.text}`;
          await tell(state.worker.id, `message/${message.id}`, text);
          return next({ help: null, stalled: null, note: text });
        }

        // Waiting on an agent's report: watch it for a stall (stall.ts).
        const awaiting = async (reason: string, ask: string) => {
          const awaited = awaitedAgent(state);
          if (!awaited) return wait(reason);
          const now = clock();
          const step = stallStep({
            key: awaited.key,
            status: current(ctx.get(activity))?.[awaited.id],
            watch: state.watch,
            now,
          });
          if (step.action === "nudge")
            await tell(
              awaited.id,
              // One nudge per idle stretch: its start is persisted, so a retried reconcile reuses it.
              `nudge/${awaited.key}/${state.watch?.idleSince ?? now}`,
              `Task #${ctx.input.issue} waits for ${ask}. You have been idle ${Math.round(STALL.idle / 60_000)} minutes. ` +
                `If it is done, call that tool now. If you are waiting for background work, call report_progress with what you wait for and the minutes it needs. ` +
                (awaited.role === "worker"
                  ? "If something blocks you, call ask_coordinator."
                  : "If something blocks the review, say so in report_review."),
            );
          const stalled =
            step.action === "escalate"
              ? `The ${awaited.role} (${awaited.id}) has stayed idle without ${ask} through ${STALL.nudges} nudges` +
                (state.progress ? `; its last progress: ${state.progress.note}` : "") +
                ". Look at its terminal, message it, or cancel the task."
              : state.stalled;
          return wait(reason, { watch: step.watch, stalled }, step.retryAt);
        };

        // Help is answered by the coordinator; it never blocks a report that follows it.
        const help = ctx.reports.find((report) => report.data.kind === "help");
        if (help && help.data.kind === "help") {
          ctx.consumeReports(help.id);
          return next({ help: help.data.message });
        }

        // Rework from landing: withdraw the change's own queue entry (a held one
        // never lands by itself), tell the worker why, and build again.
        const rework = ctx.reports.find((report) => report.data.kind === "rework");
        if (rework && rework.data.kind === "rework" && (state.phase === "landing" || state.phase === "queue")) {
          ctx.consumeReports(rework.id);
          const commit = state.commit!;
          await ctx.effect({
            key: `withdraw/${commit}/${rework.id}`,
            input: { commit },
            idempotent: true,
            async run({ signal }) {
              const found = landingFor(await landingStatus(options.root, signal), commit);
              if (!found.checking && !found.waiting && !found.held) return null;
              const withdrawn = await runNts(options.root, ["tooling/landing/check.ts", "withdraw", commit], {
                signal,
                timeout: 120_000,
              });
              if (withdrawn.code !== 0)
                throw new Error(`check.ts withdraw ${commit} exited ${withdrawn.code}: ${(withdrawn.stderr || withdrawn.stdout).trim()}`);
              return null;
            },
          });
          const note = `Rework ${commit}: ${rework.data.notes}\nFix the cause where it lives, prove the change again, commit and report_ready.`;
          await tell(state.worker!.id, `rework/${rework.id}`, note);
          return next({ phase: "build", rounds: state.rounds + 1, note, help: null });
        }

        if (
          state.worker &&
          (state.phase === "design" || state.phase === "approve" || state.phase === "build") &&
          ended(state.worker.id)
        )
          return next({ phase: "start", resumeTo: state.phase });

        if (state.phase === "prepare") {
          const found = issue(current(ctx.get(options.issues)) ?? [], ctx.input.issue);
          if (!found) return wait(`Issue #${ctx.input.issue} is not available`);
          return next({
            phase: "workspace",
            title: found.title,
            revision: hash({ body: found.body, labels: found.labels }),
            paths: cardPaths(found.body),
          });
        }
        if (state.phase === "workspace") {
          const tree = await ctx.effect<{ path: string; branch: string }>({
            key: "take-tree",
            input: { issue: ctx.input.issue },
            async recover({ signal }) {
              const held = treeForIssue(await poolStatus(options.root, signal), ctx.input.issue);
              return held
                ? { status: "completed" as const, value: { path: held.path, branch: held.task! } }
                : { status: "absent" as const };
            },
            async run({ signal }) {
              const taken = await runNts(
                options.root,
                ["tooling/landing/pool.ts", "take", "--issue", String(ctx.input.issue)],
                { signal, timeout: 600_000 },
              );
              const held = treeForIssue(await poolStatus(options.root, signal), ctx.input.issue);
              if (!held)
                throw new Error(`pool.ts take exited ${taken.code}: ${(taken.stderr || taken.stdout).trim()}`);
              return { path: held.path, branch: held.task! };
            },
          });
          return next({ phase: "start", tree: tree.path, branch: tree.branch });
        }
        if (state.phase === "start") {
          const resolved = await ctx.get(options.profiles).resolve("worker");
          if ("wait" in resolved) return wait(resolved.wait, {}, resolved.retryAt);
          const found = issue(current(ctx.get(options.issues)) ?? [], ctx.input.issue);
          if (!found) return wait(`Issue #${ctx.input.issue} is not available`);
          const generation = state.generation + 1;
          const resume = state.resumeTo
            ? workerResumeBrief({
                assignment: ctx.assignment.id,
                issue: found,
                branch: state.branch!,
                tree: state.tree!,
                phase: state.resumeTo,
                design: state.design,
                commit: state.commit,
                note: state.note,
              })
            : undefined;
          const started = await ctx.effect<{ id: string }>({
            // The first launch keeps its original key, so receipts from before generations recover.
            key: generation === 1 ? "start-worker" : `start-worker/${generation}`,
            input: { tree: state.tree, profile: resolved.profile, generation },
            idempotent: true,
            run: async ({ requestId, signal }) => {
              const instance = await options.agents.start(
                runtime,
                {
                  profile: resolved.profile,
                  label: `#${ctx.input.issue} worker`,
                  cwd: state.tree!,
                  instructions: workerInstructions(),
                  brief:
                    resume ??
                    workerBrief({
                      assignment: ctx.assignment.id,
                      issue: found,
                      branch: state.branch!,
                      tree: state.tree!,
                      instructions: ctx.input.instructions,
                    }),
                  tools: [options.workerTools],
                  requestId,
                },
                signal,
              );
              return { id: instance.id };
            },
          });
          return next({
            phase: state.resumeTo ?? "design",
            worker: { id: started.id, profile: resolved.profile },
            generation,
            resumeTo: null,
          });
        }
        if (state.phase === "design") {
          const report = ctx.reports.find((report) => report.data.kind === "design");
          if (!report || report.data.kind !== "design")
            return awaiting("Waiting for the worker's design", "your design (report_design)");
          ctx.consumeReports(report.id);
          return next({ phase: "approve", design: report.data.summary, help: null });
        }
        if (state.phase === "approve") {
          const report = ctx.reports.find((report) => report.data.kind === "design_decision");
          if (!report || report.data.kind !== "design_decision")
            return wait("Waiting for the coordinator to approve the design");
          ctx.consumeReports(report.id);
          const decision = report.data.accept
            ? `Design approved. ${report.data.notes}\nBuild it, prove it (focused tests, inner.ts, check.ts --here), commit, then report_ready.`
            : `Design returned. ${report.data.notes}\nRevise it and report_design again.`;
          await tell(state.worker!.id, `design/${report.id}`, decision);
          return next({ phase: report.data.accept ? "build" : "design", note: decision });
        }
        if (state.phase === "build") {
          const report = ctx.reports.find((report) => report.data.kind === "ready");
          if (!report || report.data.kind !== "ready")
            return awaiting("Waiting for the worker's proved commit", "your proved commit (report_ready)");
          ctx.consumeReports(report.id);
          const git = async (args: string[]) =>
            (await run("git", ["--no-optional-locks", ...args], { cwd: state.tree!, timeout: 30_000 })).stdout.trim();
          if ((await git(["rev-parse", "HEAD"])) !== report.data.commit || (await git(["status", "--porcelain"])))
            throw new Conflict("The reported commit is not the clean HEAD of the task's tree");
          return next({ phase: "review", commit: report.data.commit, summary: report.data.summary, help: null });
        }
        if (state.phase === "review") {
          const resolved = await ctx.get(options.profiles).resolve("reviewer");
          if ("wait" in resolved) return wait(resolved.wait, {}, resolved.retryAt);
          const commit = state.commit!;
          const started = await ctx.effect<{ id: string }>({
            // Each review round gets a fresh reviewer, even of the same commit.
            key: `start-reviewer/${commit}/${state.rounds}`,
            input: { tree: state.tree, commit, round: state.rounds, profile: resolved.profile },
            idempotent: true,
            run: async ({ requestId, signal }) => {
              const instance = await options.agents.start(
                runtime,
                {
                  profile: resolved.profile,
                  label: `#${ctx.input.issue} review ${commit.slice(0, 12)}`,
                  cwd: state.tree!,
                  instructions: reviewerInstructions(),
                  brief: reviewerBrief({
                    assignment: ctx.assignment.id,
                    issue: ctx.input.issue,
                    title: state.title,
                    commit,
                    tree: state.tree!,
                    design: state.design,
                    summary: state.summary,
                    previous: state.reviews,
                    labels: current(ctx.get(options.issues))?.find((issue) => issue.number === ctx.input.issue)?.labels ?? [],
                  }),
                  tools: [options.reviewerTools],
                  requestId,
                },
                signal,
              );
              return { id: instance.id };
            },
          });
          return next({ phase: "reviewing", reviewer: { id: started.id, commit, round: state.rounds, profile: resolved.profile } });
        }
        if (state.phase === "reviewing") {
          const report = ctx.reports.find(
            (report) => report.data.kind === "review" && report.data.commit === state.commit,
          );
          if (!report || report.data.kind !== "review")
            return awaiting("Waiting for the review of the exact commit", `your review of ${state.commit} (report_review)`);
          ctx.consumeReports(report.id);
          await stop(state.reviewer!.id, `reviewer/${state.reviewer!.commit}/${state.reviewer!.round}`);
          if (report.data.verdict === "accept") return next({ phase: "queue", reviewer: null });
          const reviews = [...state.reviews, { commit: state.commit!, findings: report.data.findings }];
          // A dispute of the approved design is the coordinator's call, not a
          // round of rework the worker should start on its own.
          if (report.data.disputesDesign)
            return wait("The reviewer disputes the approved design; the coordinator decides", {
              phase: "build",
              reviewer: null,
              reviews,
              help: `The reviewer of ${state.commit} disputes the approved design:\n${report.data.findings}\nDecide: message the worker to change course, or to keep the design and answer the finding.`,
            });
          const rounds = state.rounds + 1;
          const findings = `The review of ${state.commit} asks for changes:\n${report.data.findings}\nFix them, prove the change again, commit and report_ready.`;
          await tell(state.worker!.id, `review/${report.id}`, findings);
          return rounds > options.maxRounds
            ? wait(`Escalated after ${rounds} rounds; the coordinator decides`, { phase: "build", reviewer: null, rounds, note: findings, reviews })
            : next({ phase: "build", reviewer: null, rounds, note: findings, reviews });
        }
        if (state.phase === "queue") {
          const commit = state.commit!;
          // One submission per commit and round: a reworked commit queues again.
          const queuedAt = await ctx.effect<number>({
            key: state.rounds === 0 ? `queue/${commit}` : `queue/${commit}/${state.rounds}`,
            input: { commit, round: state.rounds },
            async recover({ signal }) {
              const found = landingFor(await landingStatus(options.root, signal), commit);
              return found.checking || found.waiting || found.held
                ? { status: "completed" as const, value: clock() }
                : { status: "absent" as const };
            },
            async run({ signal }) {
              const queued = await runNts(state.tree!, ["tooling/landing/check.ts", commit, "--no-wait"], {
                signal,
                timeout: 300_000,
              });
              if (queued.code !== 0)
                throw new Error(`check.ts ${commit} exited ${queued.code}: ${(queued.stderr || queued.stdout).trim()}`);
              return clock();
            },
          });
          return next({ phase: "landing", queuedAt });
        }
        if (state.phase === "landing") {
          const status = current(ctx.get(options.landing));
          if (!status) return wait("Waiting for the landing status");
          const answer = landingFor(status, state.commit!).answers.find(
            // The landing service and this host share the machine's clock.
            (candidate) => state.queuedAt === null || Date.parse(candidate.at) >= state.queuedAt,
          );
          if (!answer) return wait("Waiting for the landing check's answer");
          if (answer.state === "landed")
            return next({ phase: "verify", landedMain: status.main.commit ?? state.commit });
          if (answer.state === "held") return wait(`Landing held: ${answer.message}`);
          if (answer.state === "withdrawn") return next({ phase: "retire", outcome: "withdrawn" });
          const rounds = state.rounds + 1;
          const refusal = `The landing check refused ${state.commit}: ${answer.message}\nLogs: ${answer.run ?? "none"}\nFind the cause, fix it where it lives, prove it again, commit and report_ready.`;
          await tell(state.worker!.id, `refused/${answer.id}`, refusal);
          return rounds > options.maxRounds
            ? wait(`Escalated after ${rounds} rounds; the coordinator decides`, { phase: "build", rounds, note: refusal })
            : next({ phase: "build", rounds, note: refusal });
        }
        if (state.phase === "verify") {
          const verified = current(ctx.get(options.fullCheck))?.verified.commit;
          if (!verified) return wait("Waiting for the full check to verify the landing");
          const contained = await run(
            "git",
            ["--no-optional-locks", "merge-base", "--is-ancestor", state.landedMain!, verified],
            { cwd: options.root, timeout: 30_000 },
          ).then(
            () => true,
            () => false,
          );
          if (!contained) return wait(`Waiting for verified to reach ${state.landedMain!.slice(0, 12)}`);
          return next({ phase: "retire", outcome: "verified" });
        }
        if (state.phase === "retire") {
          if (state.worker) await stop(state.worker.id, "worker");
          if (state.reviewer) await stop(state.reviewer.id, `reviewer/${state.reviewer.commit}/${state.reviewer.round}`);
          return { state: { ...state, phase: "done" as const }, next: "finish" as const };
        }
        return { state, next: "finish" as const };
      },
      async cancel(ctx) {
        // The task's tree and branch belong to NTS's pool; only this task's sessions end here.
        const runtime = ctx.get(options.agents.runtime);
        for (const agent of [ctx.assignment.state.worker, ctx.assignment.state.reviewer])
          if (agent) {
            const end = runtime.capability(agent.id, terminate);
            if (end)
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
        return { state: { ...ctx.assignment.state, phase: "done" as const, outcome: `cancelled: ${ctx.reason}` }, next: "finish" as const };
      },
    },
  });
}
export type TaskRoute = ReturnType<typeof taskRoute>;
