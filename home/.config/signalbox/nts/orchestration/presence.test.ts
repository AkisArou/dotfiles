// The nts workspace's detach and attach (#884) against a stand-in pool.ts: each call
// exits with the next code its script names, and records what it was asked.
import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { attach, detach, take } from "./task-parts.ts";

async function pool(t: { after(fn: () => unknown): void }, codes: readonly number[]) {
  const root = await mkdtemp(join(tmpdir(), "nts-presence-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  await mkdir(join(root, "tooling/landing"), { recursive: true });
  await writeFile(join(root, "codes.json"), JSON.stringify(codes));
  await writeFile(join(root, "trees.json"), "[]");
  await writeFile(
    join(root, "tooling/landing/pool.ts"),
    `import { appendFileSync, readFileSync, writeFileSync } from "node:fs";
const codes = JSON.parse(readFileSync("codes.json", "utf8"));
if (process.argv[2] !== "status") {
  appendFileSync("calls.log", process.argv.slice(2).join(" ") + "\\n");
  writeFileSync("codes.json", JSON.stringify(codes.slice(1)));
}
if (process.argv[2] === "status") {
  const trees = JSON.parse(readFileSync("trees.json", "utf8"));
  console.log(JSON.stringify({ version: 2, pool: ".", room: { ok: true, free_bytes: 0, floor_bytes: 0, unbuilt: 0, why: null }, trees }));
} else {
  if (codes[0]) console.error("cannot " + process.argv[2] + ": a process still uses the tree");
  if (process.argv[2] === "take" && !codes[0])
    writeFileSync("trees.json", JSON.stringify([{ name: "3", path: "/pool/3", state: "held", task: process.argv[4], ahead: 0, behind: 0, uncommitted: 0, idle_ms: null, target_bytes: null }]));
  process.exitCode = codes[0] ?? 0;
}
`,
  );
  const calls = async () => (await readFile(join(root, "calls.log"), "utf8")).trim().split("\n");
  return { root, calls };
}

const signal = new AbortController().signal;

test("detach asks the pool once: it detaches the tree or records the pending detach (#1066)", async (t) => {
  const { root, calls } = await pool(t, [0]);
  await detach(root, "907", signal);
  assert.deepEqual(await calls(), ["detach 907"]);
});

test("a held tree lock (exit 75) is asked again once it ends, not a fault", async (t) => {
  const { root, calls } = await pool(t, [75, 75, 0]);
  await detach(root, "907", signal, 1);
  assert.deepEqual(await calls(), ["detach 907", "detach 907", "detach 907"]);
});

test("a tree reset after its change landed holds the branch no more: it is detached already", async (t) => {
  // trees.json is empty: no tree holds 907, as after a landing reset its tree.
  const { root, calls } = await pool(t, [1]);
  await detach(root, "907", signal);
  assert.deepEqual(await calls(), ["detach 907"]);
});

test("a detach the pool could neither do nor record is the task's fault, named, not a success", async (t) => {
  const { root, calls } = await pool(t, [1, 0]);
  await writeFile(
    join(root, "trees.json"),
    JSON.stringify([
      {
        name: "3",
        path: "/pool/3",
        state: "held",
        task: "907",
        ahead: 0,
        behind: 0,
        uncommitted: 0,
        idle_ms: null,
        target_bytes: null,
      },
    ]),
  );
  await assert.rejects(detach(root, "907", signal), /pool\.ts detach 907 exited 1: cannot detach/);
  assert.deepEqual(await calls(), ["detach 907"], "asked once: the pool owns any later retry");
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

test("take waits while the machine cannot host another tree (exit 75), then takes it", async (t) => {
  const { root, calls } = await pool(t, [75, 75, 0]);
  assert.deepEqual(await take(root, 909, signal, 1), { path: "/pool/3", branch: "909" });
  assert.deepEqual(await calls(), ["take --issue 909", "take --issue 909", "take --issue 909"]);
});

test("any other take failure is the task's, named", async (t) => {
  const { root } = await pool(t, [1]);
  await assert.rejects(take(root, 909, signal, 1), /pool\.ts take exited 1: cannot take/);
});
