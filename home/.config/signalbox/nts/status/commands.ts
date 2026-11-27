// NTS's own commands, as this application runs them: one runner, typed results.
// Readers and orchestration both go through here, so a command's arguments,
// working directory and output contract live in one place.
import { execFile } from "node:child_process";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import type { LandingStatus } from "./landing.ts";
import type { PoolStatus } from "./machine.ts";
import type { FullCheckStatus } from "./full-check.ts";
import { readFullCheck } from "./full-check.ts";
import { checked } from "./status.ts";

export interface CommandResult {
  readonly code: number;
  readonly stdout: string;
  readonly stderr: string;
}

/** Asks asdf for the `node` a directory's .tool-versions selects: its binary's path. */
function asdfNode(cwd: string): Promise<string> {
  return new Promise((resolve, reject) =>
    execFile("asdf", ["which", "node"], { cwd, timeout: 30_000 }, (error, stdout, stderr) =>
      error
        ? reject(new Error(`asdf which node in ${cwd}: ${stderr.trim() || error.message}`))
        : resolve(stdout.trim()),
    ),
  );
}

const pinned = new Map<string, { readonly pin: string; readonly node: Promise<string> }>();
/**
 * The Node the repository pins for `cwd` (its .tool-versions, as asdf resolves it there),
 * never this host's own: nts tooling records which Node ran it (a tree's installation
 * catalog names its installer), so one run on another Node writes another record. Kept
 * per directory while its pin reads the same; a pin that is not installed fails, named.
 */
export function pinnedNode(cwd: string, resolve: (cwd: string) => Promise<string> = asdfNode) {
  let pin = "";
  try {
    pin = readFileSync(join(cwd, ".tool-versions"), "utf8");
  } catch {
    // No pin of its own here: asdf resolves it from a parent, as it would for anyone.
  }
  const known = pinned.get(cwd);
  if (known && known.pin === pin) return known.node;
  const node = resolve(cwd);
  pinned.set(cwd, { pin, node });
  node.catch(() => pinned.delete(cwd));
  return node;
}

/** Runs `node <args>` in `cwd` on the repository's pinned Node. A nonzero exit is a result, not an exception. */
export async function runNts(
  cwd: string,
  args: readonly string[],
  options: { readonly signal?: AbortSignal; readonly timeout?: number } = {},
): Promise<CommandResult> {
  const node = await pinnedNode(cwd);
  return new Promise((resolve, reject) => {
    execFile(
      node,
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
        resolve({
          code: error ? Number((error as { code?: number }).code ?? 1) : 0,
          stdout,
          stderr,
        });
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

export const poolStatus = async (root: string, signal?: AbortSignal) =>
  checked<PoolStatus>(
    await json(root, ["tooling/landing/pool.ts", "status", "--json"], signal),
    "pool",
    // Version 2 (#884) adds held and landed trees' worker presence; their other
    // fields, which this application reads, are unchanged.
    [1, 2],
  );
export const landingStatus = async (root: string, signal?: AbortSignal) =>
  checked<LandingStatus>(
    await json(root, ["tooling/landing/check.ts", "status", "--json"], signal),
    "landing",
    // Version 2 (#819) drops the pool trees, which this application never reads.
    [1, 2],
    "landing",
  );
export const fullCheckStatus = async (
  root: string,
  signal?: AbortSignal,
): Promise<FullCheckStatus> =>
  readFullCheck(await json(root, ["tooling/full-check/service.ts", "--status", "--json"], signal));

/** The pool tree holding issue N's branch (`<N>` or `<N>-<slug>`), if any. */
export function treeForIssue(pool: PoolStatus, issue: number) {
  return pool.trees.find(
    (tree) =>
      tree.task !== null && (tree.task === String(issue) || tree.task.startsWith(`${issue}-`)),
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
