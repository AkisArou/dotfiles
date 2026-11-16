// NTS's own commands, as this application runs them: one runner, typed results.
// Readers and orchestration both go through here, so a command's arguments,
// working directory and output contract live in one place.
import { execFile } from "node:child_process";
import type { LandingStatus } from "./landing.ts";
import type { PoolStatus } from "./machine.ts";
import type { FullCheckStatus } from "./full-check.ts";
import { readFullCheck } from "./full-check.ts";

export interface CommandResult {
  readonly code: number;
  readonly stdout: string;
  readonly stderr: string;
}

/** Runs `node <args>` in `cwd`. A nonzero exit is a result, not an exception. */
export function runNts(
  cwd: string,
  args: readonly string[],
  options: { readonly signal?: AbortSignal; readonly timeout?: number } = {},
): Promise<CommandResult> {
  return new Promise((resolve, reject) => {
    execFile(
      process.execPath,
      [...args],
      {
        cwd,
        timeout: options.timeout ?? 60_000,
        maxBuffer: 16 * 1024 * 1024,
        ...(options.signal ? { signal: options.signal } : {}),
      },
      (error, stdout, stderr) => {
        if (error && typeof (error as NodeJS.ErrnoException).code !== "number")
          return reject(error);
        resolve({ code: error ? Number((error as { code?: number }).code ?? 1) : 0, stdout, stderr });
      },
    );
  });
}

async function json(root: string, args: readonly string[], signal?: AbortSignal): Promise<unknown> {
  const result = await runNts(root, args, { ...(signal ? { signal } : {}) });
  if (result.code !== 0)
    throw new Error(`node ${args.join(" ")} exited ${result.code}: ${result.stderr.trim()}`);
  return JSON.parse(result.stdout) as unknown;
}

export const poolStatus = (root: string, signal?: AbortSignal) =>
  json(root, ["tooling/landing/pool.ts", "status", "--json"], signal) as Promise<PoolStatus>;
export const landingStatus = (root: string, signal?: AbortSignal) =>
  json(root, ["tooling/landing/check.ts", "status", "--json"], signal) as Promise<LandingStatus>;
export const fullCheckStatus = async (root: string, signal?: AbortSignal): Promise<FullCheckStatus> =>
  readFullCheck(await json(root, ["tooling/full-check/service.ts", "--status", "--json"], signal));

/** The pool tree holding issue N's branch (`<N>` or `<N>-<slug>`), if any. */
export function treeForIssue(pool: PoolStatus, issue: number) {
  return pool.trees.find(
    (tree) => tree.task !== null && (tree.task === String(issue) || tree.task.startsWith(`${issue}-`)),
  );
}

/** Every landing entry and answer for one commit, newest first. */
export function landingFor(status: LandingStatus, sha: string) {
  const same = (value: string) => value === sha || sha.startsWith(value) || value.startsWith(sha);
  return {
    checking: status.checking && same(status.checking.sha) ? status.checking : null,
    waiting: status.waiting.find((entry) => same(entry.sha)) ?? null,
    held: status.held.find((entry) => same(entry.sha)) ?? null,
    answers: status.recent.filter((answer) => same(answer.sha)),
  };
}
