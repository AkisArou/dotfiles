// How nts does what its tasks need (the kit's task route leaves these to it): a
// tree from its pool, the coordinator's sign-off for core paths (AGENTS.md), the
// landing check, the full check's verified, and briefs in its own terms.
import { execFile } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import { promisify } from "node:util";
import { latestValue, mapReadable } from "signalbox/core";
import type { Capability, Readable } from "signalbox/core";
import type { TaskParts } from "@signalbox/kit/orchestration/task-parts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { landingFor, landingStatus, poolStatus, runNts, treeForIssue } from "../status/commands.ts";
import {
  reviewerBrief,
  reviewerInstructions,
  workerBrief,
  workerInstructions,
  workerResumeBrief,
} from "./briefs.ts";

const run = promisify(execFile);

/** Labels that keep an issue from starting: the user's, or the plan's. */
const BLOCKING = ["state:needs-user", "state:held", "state:parked"];

/** Paths AGENTS.md has the coordinator read before a change is submitted. */
export const CORE_PATHS = ["compiler/", "runtime/c/", "tooling/gate/", "tooling/landing/"];

const failed = (what: string, result: { code: number | null; stdout: string; stderr: string }) =>
  new Error(`${what} exited ${result.code}: ${(result.stderr || result.stdout).trim()}`);

/** pool.ts's exit code when the machine cannot host another tree's build yet (EX_TEMPFAIL). */
const TEMPFAIL = 75;

/**
 * A worker returns to a kept tree (#884): its build output is the task's again. When the
 * machine cannot host its build yet, it waits for room, as a fresh take does.
 */
export async function attach(
  root: string,
  branch: string,
  signal: AbortSignal,
  pauseMs = 30_000,
): Promise<void> {
  for (;;) {
    const result = await runNts(root, ["tooling/landing/pool.ts", "attach", branch], {
      signal,
      timeout: 600_000,
    });
    if (result.code === 0) return;
    if (result.code !== TEMPFAIL) throw failed(`pool.ts attach ${branch}`, result);
    await sleep(pauseMs, undefined, { signal });
  }
}

/** The pool tree holding issue N's branch, if any. */
async function held(root: string, issue: number, signal: AbortSignal) {
  const tree = treeForIssue(await poolStatus(root, signal), issue);
  return tree && { path: tree.path, branch: tree.task! };
}

/**
 * A tree for issue N from the pool, at the newest main on its branch. When the machine
 * cannot host another tree's build yet (exit 75), it waits for room and asks again, as
 * attach does: the refusal changed nothing.
 */
export async function take(
  root: string,
  issue: number,
  signal: AbortSignal,
  pauseMs = 30_000,
): Promise<{ path: string; branch: string }> {
  for (;;) {
    const taken = await runNts(
      root,
      ["tooling/landing/pool.ts", "take", "--issue", String(issue)],
      {
        signal,
        timeout: 600_000,
      },
    );
    if (taken.code === TEMPFAIL) {
      await sleep(pauseMs, undefined, { signal });
      continue;
    }
    const tree = await held(root, issue, signal);
    if (!tree) throw failed("pool.ts take", taken);
    return tree;
  }
}

/**
 * No worker works in a kept tree any more (#884): the pool may reclaim its build output
 * under disk pressure. The pool owns when that is safe (#1066): it detaches the tree at
 * once, or records a durable detach-pending request that its upkeep completes once no
 * process, unit, build claim or resource lock uses it (a worker's own nts-<N>-* unit can
 * outlive its pane), and a resume replaces the request. Either way it exits 0. A held
 * tree lock (exit 75) is asked again once it ends; any other nonzero exit, when it could do
 * neither, is the task's fault, named.
 */
export async function detach(
  root: string,
  branch: string,
  signal: AbortSignal,
  pauseMs = 30_000,
): Promise<void> {
  let result = await runNts(root, ["tooling/landing/pool.ts", "detach", branch], {
    signal,
    timeout: 120_000,
  });
  // The tree's resource lock is held (a gate run or disk reclamation uses it): the refusal
  // changed nothing, so ask again once it ends, as attach and take do (EX_TEMPFAIL).
  while (result.code === TEMPFAIL) {
    await sleep(pauseMs, undefined, { signal });
    result = await runNts(root, ["tooling/landing/pool.ts", "detach", branch], {
      signal,
      timeout: 120_000,
    });
  }
  if (result.code === 0) return;
  // A tree the pool reset once its change landed holds the branch no more: nothing is
  // left to detach (detach is idempotent). The pool's own status says so, not its words.
  const pool = await poolStatus(root, signal);
  if (!pool.trees.some((tree) => tree.task === branch)) return;
  throw failed(`pool.ts detach ${branch}`, result);
}

export function ntsTaskParts(options: {
  readonly root: string;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
}): TaskParts {
  const { root } = options;
  return {
    blocked(issue) {
      const label = issue.labels.find((name) => BLOCKING.includes(name));
      if (label) return `Issue #${issue.number} is ${label}`;
      if (!issue.labels.includes("task")) return `Issue #${issue.number} is not a task issue`;
      return undefined;
    },
    workspace: {
      find: (issue, signal) => held(root, issue, signal),
      take: (issue, signal) => take(root, issue, signal),
      detach: (workspace, signal) => detach(root, workspace.branch, signal),
      attach: (workspace, signal) => attach(root, workspace.branch, signal),
    },
    // The core paths a commit changes since it left main.
    async signoff(tree, commit) {
      const { stdout } = await run(
        "git",
        ["--no-optional-locks", "diff", "--name-only", `main...${commit}`],
        { cwd: tree, timeout: 30_000, maxBuffer: 16 * 1024 * 1024 },
      );
      return stdout
        .split("\n")
        .filter((path) => path && CORE_PATHS.some((prefix) => path.startsWith(prefix)));
    },
    landing: {
      requires: [options.landing],
      view: (get) =>
        mapReadable(get(options.landing), (status) => ({
          main: status.main.commit,
          answers: status.recent.map((answer) => ({
            id: answer.id,
            commit: answer.sha,
            state: answer.state,
            message: answer.message,
            at: Date.parse(answer.at),
            run: answer.run,
          })),
        })),
      async pending(commit, signal) {
        const found = landingFor(await landingStatus(root, signal), commit);
        return Boolean(found.checking || found.waiting || found.held);
      },
      async submit(tree, commit, signal) {
        const queued = await runNts(tree, ["tooling/landing/check.ts", commit, "--no-wait"], {
          signal,
          timeout: 300_000,
        });
        if (queued.code !== 0) throw failed(`check.ts ${commit}`, queued);
      },
      async withdraw(commit, signal) {
        const withdrawn = await runNts(root, ["tooling/landing/check.ts", "withdraw", commit], {
          signal,
          timeout: 120_000,
        });
        if (withdrawn.code !== 0) throw failed(`check.ts withdraw ${commit}`, withdrawn);
      },
    },
    // A landed change is done once the full check's verified contains it.
    verification: {
      requires: [options.fullCheck],
      watched: (get) => mapReadable(get(options.fullCheck), (status) => status.verified.commit),
      async check(get, landed) {
        const verified = latestValue(get(options.fullCheck))?.verified.commit;
        if (!verified) return "Waiting for the full check to verify the landing";
        const contained = await run(
          "git",
          ["--no-optional-locks", "merge-base", "--is-ancestor", landed, verified],
          { cwd: root, timeout: 30_000 },
        ).then(
          () => true,
          () => false,
        );
        return contained || `Waiting for verified to reach ${landed.slice(0, 12)}`;
      },
    },
    briefs: {
      workerInstructions,
      reviewerInstructions,
      worker: workerBrief,
      workerResume: workerResumeBrief,
      reviewer: reviewerBrief,
      approved: (notes) =>
        `Design approved. ${notes}\nBuild it, prove it (focused tests, inner.ts --no-agree, and your card's proof), commit, then report_ready. The landing service runs the landing check; run check.ts --here only to diagnose a landing it refused.`,
    },
  };
}
