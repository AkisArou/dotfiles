// nts's worktree pool (docs/workflow.md, The machine), for signalbox: where
// nts's agents work. nts's own tool hands out the trees, and signalbox never
// makes a worktree. `pool.ts take --issue <N>` hands out a free tree at the
// newest main, on issue N's branch, and exits 75 when the disk can host no
// other; `give` takes it back once the agent is done. Landing gives a tree
// back too, so giving one already free is harmless. The pool's trees and room
// show on the Machine tab (machine.ts).

import { CommandFailed, type Workspaces } from "signalbox";

/** `pool.ts`, with its arguments, run from nts's main checkout. */
export const pool = (args: readonly string[]) => ["node", "tooling/landing/pool.ts", ...args];

/** nts's pool, run from its main checkout: the project's first root. */
export function ntsPool(): Workspaces {
  return {
    take: async ({ task, run, roots }) => {
      // nts's branches name their issue, so only an issue's task takes a tree.
      if (!task?.source?.id.startsWith("github-issues:")) throw new Error("only a task from an nts issue takes a pool tree: a task's branch names its issue");
      try {
        // The tree's path, then "  task <branch>, at <commit>".
        const [path = "", told = ""] = (await run(pool(["take", "--issue", task.id]), { cwd: roots[0], timeoutMs: 600_000 })).split("\n");
        const branch = /task (\S+),/.exec(told)?.[1];
        return { path: path.trim(), ...(branch ? { ref: branch } : {}) };
      } catch (error) {
        if (error instanceof CommandFailed && error.exitCode === 75) return { wait: error.stderr.trim().split("\n").at(-1) || "the pool can host no other tree" };
        throw error;
      }
    },
    give: async (tree, { run, roots }) => void (await run(pool(["give", tree.ref ?? tree.path]), { cwd: roots[0], timeoutMs: 600_000 })),
  };
}
