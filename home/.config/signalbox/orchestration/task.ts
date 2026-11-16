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
import type { LandingStatus } from "../nts/landing.ts";
import type { FullCheckStatus } from "../nts/full-check.ts";
import { landingFor, landingStatus, poolStatus, runNts, treeForIssue } from "../nts/commands.ts";
import type { ManagedAgents } from "./agents.ts";
import type { Profiles } from "./profiles.ts";
import { profileSchema } from "./profiles.ts";
import { reviewerBrief, workerBrief, workerInstructions, workerResumeBrief, reviewerInstructions } from "./briefs.ts";

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
  }),
  z.object({ kind: z.literal("help"), message: z.string().min(1) }),
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
});
export type TaskState = z.infer<typeof taskState>;

/** Paths named on the card's **Paths:** line, as resources two tasks must not share. */
export function cardPaths(body: string): string[] {
  const line = body.split("\n").find((text) => /^\*\*Paths:\*\*/.test(text.trim()));
  if (!line) return [];
  const tokens = line.replace(/^\s*\*\*Paths:\*\*/, "").match(/[\w.@-]+(?:\/[\w.@*{}-]+)+/g) ?? [];
  return [...new Set(tokens.map((token) => token.replace(/[.,;:]+$/, "")))].sort();
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
}) {
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
      requires: [options.issues, options.landing, options.fullCheck, options.profiles, options.agents.runtime],
      watch: (ctx) => [ctx.get(options.landing), ctx.get(options.fullCheck), ctx.get(options.agents.runtime).instances],
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
      }),
      async reconcile(ctx) {
        const state = ctx.assignment.state;
        const runtime = ctx.get(options.agents.runtime);
        const next = (patch: Partial<TaskState>) => ({
          state: { ...state, ...patch },
          next: "continue" as const,
        });
        const wait = (reason: string, patch: Partial<TaskState> = {}, retryAt?: number) => ({
          state: { ...state, ...patch },
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
        // Help is answered by the coordinator; it never blocks a report that follows it.
        const help = ctx.reports.find((report) => report.data.kind === "help");
        if (help && help.data.kind === "help") {
          ctx.consumeReports(help.id);
          return next({ help: help.data.message });
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
          if (!report || report.data.kind !== "design") return wait("Waiting for the worker's design");
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
          if (!report || report.data.kind !== "ready") return wait("Waiting for the worker's proved commit");
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
          if (!report || report.data.kind !== "review") return wait("Waiting for the review of the exact commit");
          ctx.consumeReports(report.id);
          await stop(state.reviewer!.id, `reviewer/${state.reviewer!.commit}/${state.reviewer!.round}`);
          if (report.data.verdict === "accept") return next({ phase: "queue", reviewer: null });
          const rounds = state.rounds + 1;
          const findings = `The review of ${state.commit} asks for changes:\n${report.data.findings}\nFix them, prove the change again, commit and report_ready.`;
          await tell(state.worker!.id, `review/${report.id}`, findings);
          return rounds > options.maxRounds
            ? wait(`Escalated after ${rounds} rounds; the coordinator decides`, { phase: "build", reviewer: null, rounds, note: findings })
            : next({ phase: "build", reviewer: null, rounds, note: findings });
        }
        if (state.phase === "queue") {
          const commit = state.commit!;
          await ctx.effect({
            key: `queue/${commit}`,
            input: { commit },
            async recover({ signal }) {
              const found = landingFor(await landingStatus(options.root, signal), commit);
              return found.checking || found.waiting || found.held || found.answers.length
                ? { status: "completed" as const, value: null }
                : { status: "absent" as const };
            },
            async run({ signal }) {
              const queued = await runNts(state.tree!, ["tooling/landing/check.ts", commit, "--no-wait"], {
                signal,
                timeout: 300_000,
              });
              if (queued.code !== 0)
                throw new Error(`check.ts ${commit} exited ${queued.code}: ${(queued.stderr || queued.stdout).trim()}`);
              return null;
            },
          });
          return next({ phase: "landing" });
        }
        if (state.phase === "landing") {
          const status = current(ctx.get(options.landing));
          if (!status) return wait("Waiting for the landing status");
          const answer = landingFor(status, state.commit!).answers[0];
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
