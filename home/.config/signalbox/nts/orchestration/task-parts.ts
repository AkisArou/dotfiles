// How nts does what its tasks need (the kit's task route leaves these to it): a
// tree from its pool, the coordinator's sign-off for core paths (AGENTS.md), the
// landing check, the full check's verified, and briefs in its own terms.
import { execFile } from "node:child_process";
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

export function ntsTaskParts(options: {
  readonly root: string;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
}): TaskParts {
  const { root } = options;
  const held = async (issue: number, signal: AbortSignal) => {
    const tree = treeForIssue(await poolStatus(root, signal), issue);
    return tree && { path: tree.path, branch: tree.task! };
  };
  return {
    blocked(issue) {
      const label = issue.labels.find((name) => BLOCKING.includes(name));
      if (label) return `Issue #${issue.number} is ${label}`;
      if (!issue.labels.includes("task")) return `Issue #${issue.number} is not a task issue`;
      return undefined;
    },
    workspace: {
      find: held,
      async take(issue, signal) {
        const taken = await runNts(
          root,
          ["tooling/landing/pool.ts", "take", "--issue", String(issue)],
          { signal, timeout: 600_000 },
        );
        const tree = await held(issue, signal);
        if (!tree) throw failed("pool.ts take", taken);
        return tree;
      },
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
        `Design approved. ${notes}\nBuild it, prove it (focused tests, inner.ts, check.ts --here), commit, then report_ready.`,
    },
  };
}
