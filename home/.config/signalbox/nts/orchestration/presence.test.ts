// The nts workspace's detach and attach (#884) against a stand-in pool.ts: each call
// exits with the next code its script names, and records what it was asked.
import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { attach, detach } from "./task-parts.ts";

async function pool(t: { after(fn: () => unknown): void }, codes: readonly number[]) {
  const root = await mkdtemp(join(tmpdir(), "nts-presence-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  await mkdir(join(root, "tooling/landing"), { recursive: true });
  await writeFile(join(root, "codes.json"), JSON.stringify(codes));
  await writeFile(
    join(root, "tooling/landing/pool.ts"),
    `import { appendFileSync, readFileSync, writeFileSync } from "node:fs";
const codes = JSON.parse(readFileSync("codes.json", "utf8"));
appendFileSync("calls.log", process.argv.slice(2).join(" ") + "\\n");
writeFileSync("codes.json", JSON.stringify(codes.slice(1)));
if (codes[0]) console.error("cannot " + process.argv[2] + ": a process still uses the tree");
process.exitCode = codes[0] ?? 0;
`,
  );
  const calls = async () => (await readFile(join(root, "calls.log"), "utf8")).trim().split("\n");
  return { root, calls };
}

const signal = new AbortController().signal;

test("detach retries while something still uses the tree, then holds", async (t) => {
  const { root, calls } = await pool(t, [1, 1, 0]);
  await detach(root, "907", signal, 1);
  assert.deepEqual(await calls(), ["detach 907", "detach 907", "detach 907"]);
});

test("a tree that never frees up stays attached: detach gives up without failing the task", async (t) => {
  const { root, calls } = await pool(t, Array(10).fill(1));
  await detach(root, "907", signal, 1);
  assert.equal((await calls()).length, 8, "eight tries, then the tree stays attached");
});

test("attach waits while the machine cannot host the tree's build, then attaches", async (t) => {
  const { root, calls } = await pool(t, [75, 75, 0]);
  await attach(root, "907", signal, 1);
  assert.deepEqual(await calls(), ["attach 907", "attach 907", "attach 907"]);
});

test("any other attach failure is the task's, named", async (t) => {
  const { root } = await pool(t, [1]);
  await assert.rejects(
    attach(root, "907", signal, 1),
    /pool\.ts attach 907 exited 1: cannot attach/,
  );
});
