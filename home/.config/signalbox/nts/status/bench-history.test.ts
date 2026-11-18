import { test } from "node:test";
import assert from "node:assert/strict";
import {
  caseNoise,
  changes,
  machineDrift,
  progressTable,
  runOf,
  sparkline,
} from "./bench-history.ts";
import type { Run } from "./bench-history.ts";
import { cellText } from "./model.ts";

/** A run of three cases; `speed` scales every time (a machine faster or slower that night). */
function run(
  day: number,
  speed: number,
  nts: Record<string, number>,
  workloads: Record<string, string> = {},
): Run {
  const cell = (ns: number) => ({ status: "OK", nanoseconds: ns, spread: 1.01 });
  const rows = Object.entries(nts).map(([name, ns]) => ({
    name,
    workloadIdentity: workloads[name] ?? `w-${name}`,
    cells: {
      "nts C": cell(ns * speed),
      "nts LLVM": cell(ns * speed),
      "nts JVM": cell(ns * 2 * speed),
      "C++": cell(100 * speed),
      Java: cell(200 * speed),
      node: cell(250 * speed),
    },
    verdicts: { "nts C": { node: { kind: "WON", ratio: ns / 250 } } },
  }));
  return runOf({
    schemaVersion: 1,
    complete: true,
    id: `run-${day}`,
    startedAt: `2026-11-${String(day).padStart(2, "0")}T06:00:00Z`,
    compiler: { commit: `c${day}`.padEnd(40, "0") },
    provenance: { kind: "official nts-bench" },
    comparators: { node: { version: "v26" } },
    rows,
  })!;
}

test("a run file is read only when it is a complete official run", () => {
  assert.equal(
    runOf({
      schemaVersion: 1,
      complete: false,
      provenance: { kind: "official nts-bench" },
      rows: [],
    }),
    null,
  );
  assert.equal(
    runOf({ schemaVersion: 1, complete: true, provenance: { kind: "diagnostic" }, rows: [] }),
    null,
  );
  assert.equal(run(1, 1, { a: 100 }).cases.get("a")?.times["nts C"]?.ns, 100);
});

test("a machine that ran faster changes nothing; a case that slowed beyond its learnt noise does", () => {
  // Six steady nights, steady apart from a jittery case that moves ±8% each night.
  const nights = [1, 1.02, 0.98, 1.01, 0.99, 1].map((speed, index) =>
    run(index + 1, speed, { steady: 100, jittery: index % 2 ? 108 : 100, slowed: 100 }),
  );
  // The seventh night the whole machine runs 10% faster, and one case regresses 40%.
  const tonight = run(7, 0.9, { steady: 100, jittery: 108, slowed: 140 });
  const runs = [...nights, tonight];
  assert.ok(Math.abs(machineDrift(nights.at(-1)!, tonight, "C++") - 0.9) < 1e-9);
  const found = changes(runs);
  assert.deepEqual(
    found.map((each) => [each.name, each.backend, Number(each.change.toFixed(2))]),
    [
      ["slowed", "C", 1.4],
      ["slowed", "LLVM", 1.4],
      ["slowed", "JVM", 1.4],
    ],
    "the faster machine and the jittery case's usual move are not changes",
  );
  assert.ok(
    caseNoise(runs, "jittery", "nts C")! > 1.08,
    "the jittery case's noise is learnt from its history",
  );
  assert.equal(
    caseNoise(runs.slice(0, 4), "steady", "nts C"),
    undefined,
    "too few earlier runs to judge",
  );
});

test("a case is compared only between runs of the same workload", () => {
  const runs = [1, 2, 3, 4, 5, 6].map((day) => run(day, 1, { reworked: 100 }));
  runs.push(run(7, 1, { reworked: 300 }, { reworked: "w-new" }));
  assert.deepEqual(changes(runs), []);
});

test("against each comparator: the geometric mean now, last run, the change and a trend", () => {
  const runs = [run(1, 1, { a: 125, b: 250 }), run(2, 1, { a: 100, b: 250 })];
  const table = progressTable(runs)!;
  const row = table.rows.find(
    (each) => cellText(each.backend) === "C" && cellText(each.versus) === "node",
  )!;
  // geomean(0.5, 1) = 0.71 before; geomean(0.4, 1) = 0.63 now.
  assert.equal(cellText(row.now), "0.63");
  assert.equal(cellText(row.before), "0.71");
  assert.equal(cellText(row.change), "−10.6%");
  assert.equal(cellText(row.trend), "█▁");
  assert.equal(sparkline([3, undefined, 1]), "█ ▁");
});
