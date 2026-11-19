import { test } from "node:test";
import assert from "node:assert/strict";
import {
  caseNoise,
  caseProgress,
  changes,
  changesTable,
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
  // A regression's noise ends at +x%; a case's band says both ends, which differ.
  const table = changesTable(runs)!;
  assert.match(cellText(table.rows[0]!.noise), /^\+\d+\.\d%$/);
  assert.match(caseProgress(runs, "jittery")!.band!, /^\+\d+\.\d \/ −\d+\.\d%$/);
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
  assert.equal(
    cellText(row.trend),
    "█▁ best 0.63 · worst 0.71",
    "the glyphs and the best and worst they span",
  );
  // Its own language: C over C++ (100 ns in each run): geomean(1, 2.5), with no won or lost.
  const own = table.rows.find(
    (each) => cellText(each.backend) === "C" && cellText(each.versus) === "C++ (own language)",
  )!;
  assert.equal(cellText(own.now), "1.58");
  assert.equal(cellText(own.won), "—");
  assert.equal(sparkline([3, undefined, 1]), "█ ▁");
});

test("a case's runs, newest first, with real times and ratios; another workload says so", async () => {
  const { caseHistoryTable } = await import("./bench-history.ts");
  const runs = [
    run(1, 1, { a: 200 }, { a: "w-old" }),
    run(2, 1, { a: 125 }),
    run(3, 1, { a: 100 }),
  ];
  const table = caseHistoryTable(runs, "a")!;
  assert.equal(table.title, "a: its last 3 runs, newest first");
  assert.deepEqual(
    table.rows.map((row) => [cellText(row.run), cellText(row.c), cellText(row.cNode)]),
    [
      ["2026-11-03", "100.0 ns ±1.0%", "0.40"],
      ["2026-11-02", "125.0 ns ±1.0%", "0.50"],
      ["2026-11-01 (another workload)", "200.0 ns ±1.0%", "0.80"],
    ],
  );
  assert.equal(caseHistoryTable(runs, "missing"), undefined);
});

test("every row of the Benchmarks report has its own id, so up and down step through each", async () => {
  const { benchmarkReport } = await import("./records.ts");
  const { tableRows } = await import("./model.ts");
  const runs = [run(1, 1, { a: 125, b: 250 }), run(2, 1, { a: 100, b: 250 })];
  const ids = tableRows(benchmarkReport({ run: null, counts: null, rows: [] }, 0, runs)).map(
    (row) => row.id,
  );
  assert.equal(new Set(ids).size, ids.length, `duplicates in ${ids.join(", ")}`);
  assert.ok(ids.includes("bench-progress/C vs Bun AOT"));
});

test("a peak-memory row is read by its bytes, left out of the headline and the machine's drift", async () => {
  const { caseHistoryTable, changes, progressTable } = await import("./bench-history.ts");
  const peak = (day: number, speed: number, bytes: number) => {
    const base = run(day, speed, { a: 100 });
    const file = {
      schemaVersion: 1,
      complete: true,
      id: `peak-${day}`,
      startedAt: base.startedAt,
      compiler: { commit: base.commit },
      provenance: { kind: "official nts-bench" },
      comparators: { node: { version: "v26" } },
      rows: [
        ...[...base.cases].map(([name, each]) => ({
          name,
          workloadIdentity: each.workload,
          cells: Object.fromEntries(
            Object.entries(each.times).map(([column, time]) => [
              column,
              { status: "OK", nanoseconds: time.ns, spread: 1.01 },
            ]),
          ),
        })),
        {
          name: "rss",
          workloadIdentity: "w-rss",
          kind: "peak-rss" as const,
          cells: {
            // Its time moves with the machine; its bytes are what it is ranked by.
            "nts C": {
              status: "OK",
              nanoseconds: 5000 * speed,
              spread: 1.01,
              peakRss: { bytes, spread: 1.01 },
            },
            "C++": {
              status: "OK",
              nanoseconds: 5000 * speed,
              spread: 1.01,
              peakRss: { bytes: 1 << 20, spread: 1.01 },
            },
            node: {
              status: "OK",
              nanoseconds: 9000 * speed,
              spread: 1.01,
              peakRss: { bytes: 40 << 20, spread: 1.01 },
            },
          },
        },
      ],
    };
    return runOf(file)!;
  };
  const runs = [1, 2, 3, 4, 5, 6].map((day) => peak(day, 1, 4 << 20));
  runs.push(peak(7, 0.8, 6 << 20));
  const latest = runs.at(-1)!.cases.get("rss")!;
  assert.equal(latest.kind, "peak-rss");
  assert.equal(latest.times["nts C"]!.ns, 6 << 20, "its bytes, not its time");
  const found = changes(runs).find((each) => each.name === "rss" && each.backend === "C");
  assert.ok(
    found && Math.abs(found.change - 1.5) < 1e-9,
    "+50% bytes, not corrected by the faster night",
  );
  const headline = progressTable(runs)!.rows.find((row) => cellText(row.pair) === "C vs node")!;
  assert.equal(cellText(headline.now), "0.40", "the headline is the throughput case alone");
  const history = caseHistoryTable(runs, "rss")!;
  assert.match(history.title, /peak memory/);
  assert.equal(cellText(history.rows[0]!.c), "6.0 MiB ±1.0%");
});
