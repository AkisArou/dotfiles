import { test } from "node:test";
import assert from "node:assert/strict";
import { cardPaths } from "./task.ts";
import { wakeups } from "./wakeups.ts";
import type { Assignment } from "signalbox/ext/assignments/index";

test("a card's Paths line names the files a task reserves, and nothing else", () => {
  const body = [
    "**Goal:** something",
    "**Paths:** tooling/gate/disk-capacity.ts, tooling/gate/step-result.ts, the full check's verdict owner under tooling/full-check/, docs/workflow.md, their tests",
    "**Done when:** x",
  ].join("\n");
  assert.deepEqual(cardPaths(body), [
    "docs/workflow.md",
    "tooling/full-check",
    "tooling/gate/disk-capacity.ts",
    "tooling/gate/step-result.ts",
  ]);
  assert.deepEqual(cardPaths("no paths line"), []);
});

const task = (patch: object, assignment: Partial<Assignment> = {}): Assignment =>
  ({
    id: "a1",
    key: "issue/812",
    route: "nts/task",
    acceptedAt: 0,
    input: { issue: 812, instructions: "" },
    status: "active",
    heldResources: [],
    state: {
      phase: "design",
      title: "t",
      revision: "r",
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
      ...patch,
    },
    ...assignment,
  }) as unknown as Assignment;

test("wake-ups have stable identities derived from what they announce", () => {
  const observe = (assignments: Assignment[]) =>
    wakeups({ assignments, landing: undefined, fullCheck: undefined, decided: [] }).map((event) => event.id);
  const design = observe([task({ phase: "approve", design: "fix it at the cause" })]);
  assert.equal(design.length, 1);
  assert.match(design[0]!, /^design\/a1\//);
  // The same design observed again is the same event; a revised design is a new one.
  assert.deepEqual(observe([task({ phase: "approve", design: "fix it at the cause" })]), design);
  assert.notDeepEqual(observe([task({ phase: "approve", design: "a better design" })]), design);
  assert.deepEqual(observe([task({ help: "which owner?" })]).length, 1);
  assert.deepEqual(
    observe([task({ rounds: 5 }, { wait: { reason: "Escalated after 5 rounds; the coordinator decides" } })]),
    ["escalated/a1/5"],
  );
  assert.deepEqual(observe([task({ phase: "done", outcome: "verified" }, { status: "finished" })]), ["finished/a1"]);
});

test("each area's reading list names workflow sections and lessons that exist", async () => {
  const { readFile, access } = await import("node:fs/promises");
  const { readingList } = await import("./briefs.ts");
  const { ntsConfig } = await import("../config.ts");
  const workflow = await readFile(`${ntsConfig.root}/docs/workflow.md`, "utf8");
  const headings = new Set([...workflow.matchAll(/^## (.+)$/gm)].map((match) => match[1]!));
  for (const area of ["tooling", "compiler", "runtime", "jvm", "wasm", "node", "benchmarks", "chromium", "docs"]) {
    const list = readingList([`area:${area}`]);
    for (const [, name] of list.matchAll(/"## ([^"]+)"/g)) assert.ok(headings.has(name!), `${area}: no section ${name}`);
    const lessons = /^- docs\/contributing\/lessons\/: (.+)\.$/m.exec(list)?.[1] ?? "";
    for (const [, topic] of lessons.matchAll(/(\w+)\.md/g))
      await access(`${ntsConfig.root}/docs/contributing/lessons/${topic}.md`);
  }
  assert.match(readingList(["area:tooling"]), /concurrency\.md, fixtures\.md, machine\.md/);
  assert.doesNotMatch(readingList(["area:docs"]), /lessons/);
});
