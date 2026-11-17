import { test } from "node:test";
import assert from "node:assert/strict";
import { sampleReports } from "./tests/fixtures.ts";
import { cellText, gaugeState, tableRows } from "./model.ts";
import { conformanceReport } from "./records.ts";
import { readFullCheck } from "./full-check.ts";

test("NTS benchmark and census projections preserve comparators, coverage, ratios and record ages", () => {
  const reports = sampleReports(Date.parse("2026-11-16T10:00:00Z"));
  const benchmarks = reports.Benchmarks;
  const ratios = benchmarks.tables.find((table) => table.id === "bench-rows")!;
  assert.equal(ratios.rows[0]?.case, "cyclic-churn");
  assert.equal(cellText(ratios.rows[0]?.c), "1.80");
  assert.equal(typeof ratios.rows[0]?.c === "object" && ratios.rows[0].c.color, "red");
  const comparisons = benchmarks.tables.find((table) => table.id === "bench-counts")!;
  assert.equal(comparisons.rows.length, 12);
  assert.ok(comparisons.rows.some((row) => row.backend === "LLVM" && row.versus === "Bun AOT"));
  const conformance = reports.Conformance;
  const suite = conformance.tables.find((table) => table.id === "conformance-suite")!;
  assert.ok(
    suite.rows.some(
      (row) => cellText(row.record) === "intl402" && cellText(row.why).includes("no census"),
    ),
  );
  assert.match(cellText(suite.rows[0]?.why), /sloppy-only/);
  assert.match(cellText(suite.rows[0]?.noVerdict), /5/);
  assert.ok(
    conformance.tables
      .find((table) => table.id === "conformance")!
      .rows.some((row) => cellText(row.record) === "C test262 frontend negatives"),
  );
  assert.ok(
    conformance.tables
      .flatMap((table) => table.rows)
      .some((row) => cellText(row.written).includes("140 compiler commits")),
  );
  for (const report of Object.values(reports))
    assert.equal(
      new Set(tableRows(report).map((row) => row.id)).size,
      tableRows(report).length,
      "row identity must remain unique across all tables",
    );
});
test("NTS health retains project thresholds, unknown ages, and unsupported format errors", () => {
  const reports = sampleReports();
  assert.equal(
    gaugeState(reports["Full check"].gauges.find((gauge) => gauge.id === "controller")!),
    "red",
  );
  assert.equal(
    gaugeState(reports.Machine.gauges.find((gauge) => gauge.id === "memory.landing-wait")!),
    "red",
  );
  const report = conformanceReport(
    {
      backends: {
        c: { test262: { slices: { language: { under: "test/language", commit: "unknown" } } } },
        llvm: {},
        jvm: {},
      },
    },
    new Map(),
  );
  assert.equal(report.gauges[0]?.text, "unknown");
  assert.ok(
    report.tables
      .flatMap((table) => table.rows)
      .some((row) => cellText(row.written) === "age unknown"),
  );
  assert.throws(() => readFullCheck({ version: 3, kind: "full-check" }), /unsupported/);
  assert.throws(
    () => readFullCheck({ version: 1, kind: "full-check", core: {} }),
    /per-part records/,
  );
});

test("a landing answer says when it ran: start to end, with the date only when not today", async () => {
  const { ranAt } = await import("../status/landing.ts");
  const end = new Date(2026, 10, 17, 10, 42).getTime();
  assert.equal(ranAt({ at: new Date(end).toISOString(), wall_s: 660 }, end), "10:31–10:42");
  assert.equal(
    ranAt({ at: new Date(end).toISOString(), wall_s: 660 }, end + 86_400_000),
    "11-17 10:31–10:42",
  );
  assert.equal(ranAt({ at: new Date(end).toISOString(), wall_s: null }, end), "10:42");
  assert.equal(ranAt({ at: "not a time", wall_s: 1 }, end), "");
});

test("the landing page lists its recent answers, the most recent first", async () => {
  const { newestFirst } = await import("./landing.ts");
  const answers = [
    { id: "a", at: "2026-11-17T14:00:00Z" },
    { id: "b", at: "2026-11-17T16:30:00Z" },
    { id: "c", at: "2026-11-17T09:15:00Z" },
  ];
  assert.deepEqual(
    newestFirst(answers).map((answer) => answer.id),
    ["b", "a", "c"],
  );
  assert.deepEqual(
    answers.map((answer) => answer.id),
    ["a", "b", "c"],
    "the input is left as it was",
  );
});
