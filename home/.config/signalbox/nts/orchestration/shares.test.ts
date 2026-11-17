// Worker shares: a freed slot goes to the group furthest below its share,
// urgency orders tasks within a group, a share of 0 waits behind every other
// group, and the file is refused when shares overlap or leave an area out.
import { test } from "node:test";
import assert from "node:assert/strict";
import type { Assignment } from "signalbox/ext/assignments";
import type { Issue } from "../shared/model.ts";
import { parseSettings } from "./settings.ts";
import { groupOf, percents, shareRank, uncoveredAreas, withPercent } from "./shares.ts";

const shares = [
  { name: "compiler", share: 1, areas: ["compiler", "runtime"] },
  { name: "tooling", share: 1, areas: ["tooling", "docs"] },
];
const task = (issue: number, holds: boolean) =>
  ({
    input: { issue },
    heldResources: holds ? [{ resource: "worker", units: 1 }] : [],
  }) as unknown as Assignment;
const areas: Record<number, string> = {
  1: "compiler",
  2: "compiler",
  3: "tooling",
  4: "runtime",
  9: "unlabelled",
};
const placeOf = (assignment: Assignment) => ({
  area: areas[(assignment.input as { issue: number }).issue],
});
const rank = (assignment: Assignment, all: Assignment[], urgency = 0, split = shares) =>
  shareRank({ shares: split, assignment, all, placeOf, urgency });

test("a freed slot goes to the group furthest below its share; urgency orders within a group", () => {
  const running = [task(1, true)]; // compiler holds one worker, tooling none
  const compiler = task(2, false);
  const tooling = task(3, false);
  assert.ok(
    rank(tooling, running) < rank(compiler, running),
    "tooling runs nothing, so it goes first",
  );
  assert.ok(
    rank(tooling, running) < rank(compiler, running, -9),
    "even the most urgent compiler task waits while tooling is below its share",
  );
  const runtime = task(4, false);
  assert.ok(
    rank(runtime, running, -5) < rank(compiler, running, 0),
    "within compiler, urgency first",
  );
  // A share of nothing takes a slot only when no other group waits.
  const background = [
    { name: "compiler", share: 0, areas: ["compiler", "runtime"] },
    { name: "tooling", share: 1, areas: ["tooling", "docs"] },
  ];
  assert.ok(
    rank(tooling, [task(3, true), task(3, true)], 0, background) <
      rank(compiler, [], -9, background),
  );
  // An issue without an area label ranks as if its group ran nothing; without shares, urgency alone.
  assert.equal(rank(task(9, false), running, 2), 2);
  assert.equal(
    shareRank({ shares: undefined, assignment: compiler, all: running, placeOf, urgency: -3 }),
    -3,
  );
});

test("a 70/30 split gives the larger group more slots once there are enough of them", () => {
  const split = [
    { name: "compiler", share: 70, areas: ["compiler", "runtime"] },
    { name: "tooling", share: 30, areas: ["tooling", "docs"] },
  ];
  // Give out three slots one at a time to whichever group ranks first.
  const all: Assignment[] = [];
  const handed: string[] = [];
  for (let slot = 0; slot < 3; slot++) {
    const compiler = task(1, false);
    const tooling = task(3, false);
    const next = rank(compiler, all, 0, split) <= rank(tooling, all, 0, split) ? compiler : tooling;
    handed.push(next === compiler ? "compiler" : "tooling");
    all.push({ ...next, heldResources: [{ resource: "worker", units: 1 }] } as Assignment);
  }
  assert.deepEqual(handed.sort(), ["compiler", "compiler", "tooling"]);
});

test("setting one share to a percent scales the others to the rest", () => {
  assert.deepEqual(
    withPercent(shares, "compiler", 70).map((group) => group.share),
    [70, 30],
  );
  const three = [...shares, { name: "docs", share: 2, areas: ["x"] }];
  const scaled = withPercent(three, "compiler", 40);
  assert.deepEqual(
    scaled.map((group) => group.share),
    [40, 20, 40],
    "1:2 kept for the rest",
  );
  assert.equal(
    scaled.reduce((sum, group) => sum + group.share, 0),
    100,
  );
  assert.throws(() => withPercent(shares, "nope", 10), /No share named nope/);
  assert.deepEqual(
    [...percents(shares)],
    [
      ["compiler", 50],
      ["tooling", 50],
    ],
  );
});

test("settings.json is refused when shares overlap, leave a known area out, or are all 0", () => {
  const file = (split: unknown) =>
    JSON.stringify({
      workerSlots: 2,
      maxRounds: 4,
      roles: Object.fromEntries(
        ["coordinator", "worker", "reviewer"].map((role) => [
          role,
          {
            pick: "first",
            models: [{ harness: "claude", model: "claude-opus-5-5", effort: "high" }],
          },
        ]),
      ),
      shares: split,
    });
  const known = ["compiler", "runtime", "tooling", "docs"];
  assert.equal(parseSettings(file(shares), known).shares?.length, 2);
  assert.throws(
    () => parseSettings(file(shares), [...known, "wasm"]),
    /no group has the area wasm/,
  );
  assert.throws(
    () => parseSettings(file([...shares, { name: "x", share: 1, areas: ["docs"] }]), known),
    /area docs is in two groups/,
  );
  assert.throws(
    () => parseSettings(file(shares.map((group) => ({ ...group, share: 0 }))), known),
    /every share is 0/,
  );
  assert.deepEqual(uncoveredAreas(shares, known), []);
});

test("a group may take whole streams: a task joins its stream's group before its area's", () => {
  const split = [
    { name: "foundations", share: 2, areas: [], streams: [468] },
    { name: "correctness", share: 1, areas: ["compiler", "runtime"], streams: [467] },
    { name: "tooling", share: 1, areas: ["tooling", "docs"], streams: [472] },
  ];
  assert.equal(groupOf(split, { area: "compiler", stream: 468 })?.name, "foundations");
  assert.equal(groupOf(split, { area: "compiler", stream: 467 })?.name, "correctness");
  assert.equal(
    groupOf(split, { area: "compiler", stream: 472 })?.name,
    "tooling",
    "the stream wins over the area",
  );
  assert.equal(
    groupOf(split, { area: "runtime", stream: 471 })?.name,
    "correctness",
    "an unnamed stream falls back to its area",
  );
  assert.equal(groupOf(split, { area: "docs", stream: null })?.name, "tooling");
  const known = ["compiler", "runtime", "tooling", "docs"];
  const file = (shares: unknown) =>
    JSON.stringify({
      workerSlots: 4,
      maxRounds: 4,
      roles: Object.fromEntries(
        ["coordinator", "worker", "reviewer"].map((role) => [
          role,
          {
            pick: "first",
            models: [{ harness: "claude", model: "claude-opus-5-5", effort: "high" }],
          },
        ]),
      ),
      shares,
    });
  assert.doesNotThrow(() => parseSettings(file(split), known));
  assert.throws(
    () =>
      parseSettings(
        file([...split, { name: "again", share: 1, areas: [], streams: [468] }]),
        known,
      ),
    /stream #468 is in two groups/,
  );
  assert.throws(
    () => parseSettings(file([...split, { name: "empty", share: 1, areas: [] }]), known),
    /empty names no stream and no area/,
  );
});

test("the settings view counts with the issues last read, so GitHub being down never takes it away", async () => {
  const { Cell } = await import("signalbox/core");
  const { lastIssues } = await import("./endpoints.ts");
  const read = new Cell<Issue[]>();
  const view = lastIssues(read);
  assert.deepEqual(view.get(), { status: "ready", value: [], observedAt: 0 }, "not read yet: none");
  const issues = [{ number: 1 }] as unknown as Issue[];
  read.set(issues);
  assert.equal(view.get().status, "ready");
  read.fail(new Error("dial tcp: i/o timeout"));
  const reading = view.get();
  assert.ok(
    reading.status === "ready" && reading.value === issues,
    "a failed read keeps the last list",
  );
});
