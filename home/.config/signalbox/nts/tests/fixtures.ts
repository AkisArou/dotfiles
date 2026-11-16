// SYNTHETIC NTS OBSERVATIONS: exercise the real projection functions without an NTS checkout.
import { benchmarkReport, conformanceReport } from "../records.ts";
import type { BenchmarksSummary, ConformanceSummary } from "../records.ts";
import { fullCheckReport } from "../full-check.ts";
import { machineReport } from "../machine.ts";
import { landingReport } from "../landing.ts";
import type { NtsPage, Report } from "../model.ts";

export function sampleReports(now = Date.now()): Record<NtsPage, Report> {
  const iso = (hours: number) => new Date(now - hours * 3_600_000).toISOString();
  const commit = "a".repeat(40),
    previous = "b".repeat(40),
    censusCommit = "c".repeat(40);
  type Backend = "c" | "llvm" | "jvm";
  type Comparator = "node" | "Bun JIT" | "Bun AOT" | "ScriptC";
  const byBackend = <T>(make: (backend: Backend, index: number) => T): Record<Backend, T> => ({
    c: make("c", 0),
    llvm: make("llvm", 1),
    jvm: make("jvm", 2),
  });
  const byComparator = <T>(
    make: (name: Comparator, index: number) => T,
  ): Record<Comparator, T> => ({
    node: make("node", 0),
    "Bun JIT": make("Bun JIT", 1),
    "Bun AOT": make("Bun AOT", 2),
    ScriptC: make("ScriptC", 3),
  });

  const verdict = (ratio: number) => ({
    kind: ratio < 0.8 ? ("WON" as const) : ratio < 1 ? ("SHORT" as const) : ("LOSS" as const),
    ratio,
    tolerance: 0.05,
  });
  const rows = ["clbg-spectral-norm", "cyclic-churn", "array-mutations", "json-parse"].map(
    (name, index) => ({
      name,
      backends: byBackend((_backend, b) => ({
        versus: byComparator((_name, c) =>
          verdict([0.65, 1.8, 0.45, 0.95][index]! + b * 0.07 + c * 0.03),
        ),
        slowerThanBest: index === 1,
      })),
    }),
  );
  const benchmarks: BenchmarksSummary = {
    run: {
      id: "sample-run",
      startedAt: iso(3),
      finishedAt: iso(2),
      compiler: { commit },
      machine: { conditions: { cpu: "4", sibling: 5, start: { load: "1.2 1.1 0.9" } } },
      comparators: {
        node: { version: "sample node", commit: null },
        "Bun JIT": { version: "sample Bun", commit: null },
        "Bun AOT": { version: "sample Bun AOT", commit: null },
        ScriptC: { version: "sample ScriptC", commit: null },
      },
    },
    counts: byBackend((backend) =>
      byComparator((comparator) => {
        const counts: Partial<Record<ReturnType<typeof verdict>["kind"], number>> = {};
        for (const row of rows) {
          const kind = row.backends[backend].versus[comparator].kind;
          counts[kind] = (counts[kind] ?? 0) + 1;
        }
        return counts;
      }),
    ),
    rows,
  };
  const outcomes = { pass: 35, fail: 5, refused: 40, unsupported: 15, "no-verdict": 5 };
  const census = {
    selected: 105,
    cases: 100,
    attempted: 85,
    outcomes,
    negativesAccepted: 0,
    notAttempted: { "scope:initial-lane:no-strict": 10, "harness:$262": 5 },
    refused: { "lowering (NTS1xxx)": 35, "checker (TS)": 5 },
    degraded: true,
  };
  const noOutcomes: Readonly<Record<string, number>> = {};
  const conformance: ConformanceSummary = {
    census: {
      run: "sample-wide-run",
      commit: censusCommit,
      at: iso(4),
      unmeasured: { "test/intl402": 20 },
      backends: byBackend(() => ({
        ...census,
        slices: { language: { ...census, under: "test/language" } },
      })),
    },
    backends: byBackend<ConformanceSummary["backends"]["c"]>((backend) => ({
      test262: {
        cases: 50,
        outcomes: { pass: 42, fail: 8 },
        slices: { language: { under: "test/language", commit: previous } },
        runtime: { cases: 40, outcomes: { pass: 35, fail: 5 } },
        frontend: {
          cases: backend === "c" ? 10 : 0,
          outcomes: backend === "c" ? { pass: 7, fail: 3 } : noOutcomes,
        },
      },
      typescript: {
        cases: 20,
        outcomes: { pass: 15, wrong: 3, "runtime-abort": 2 },
        pending: 2,
        commit,
      },
      benchmarks: {
        scopes: { compile: { outcomes: { pass: 18, refused: 2 } } },
        pending: 1,
        commit,
      },
      benchmarkRuns: { outcomes: { pass: 3, wrong: 1 }, commit },
      dex: backend === "jvm" ? { outcomes: { pass: 9, fail: 1 }, commit } : undefined,
    })),
  };
  const run = {
    commit: previous,
    run: "sample/core",
    verdict: "green" as const,
    at: iso(3),
    failed: [],
  };
  const full = fullCheckReport(
    {
      controller: { running: false },
      now: null,
      main: commit,
      verified: { commit: previous, pushed: previous },
      behind: { commits: 6, oldest_landed_at: iso(1.5), delay_s: 5400 },
      core: {
        last_run: run,
        last_green: run,
        records: {
          listed: { pending: 2, accepted: 1 },
          last: { commit, at: iso(2), submitted: null, result: "landed", outcome: "landed" },
        },
      },
      wide: {
        last_run: {
          ...run,
          verdict: "held",
          reason: "sample memory pressure",
          failed: ["test262-census"],
        },
        last_green: { ...run, at: iso(8) },
        slot: null,
        records: { listed: null, last: null },
      },
      culprits: [],
      benchmarks: {
        last: { commit, at: iso(2), result: "landed", measured: true, outcome: "landed" },
        slower: ["cyclic-churn"],
        due: null,
      },
      count: {
        at: iso(0.4),
        verdict: "FAIL",
        lines: [
          "sample branch is not named by a task in docs/plan.md",
          "sample records folder awaits a retention decision",
        ],
      },
      tasks: [{ at: iso(1), line: "- Investigate the sample cyclic-churn regression" }],
    },
    now,
  );
  const gib = 1024 ** 3;
  const pool = {
    pool: "sample/nts-pool",
    room: { ok: true, free_bytes: 80e9, floor_bytes: 20e9, unbuilt: 0, why: null },
    trees: [
      {
        name: "1",
        path: "sample/nts-pool/1",
        state: "held",
        task: "101-separators",
        ahead: 1,
        behind: 0,
        uncommitted: 0,
        idle_ms: 0,
        target_bytes: 3e9,
      },
      {
        name: "2",
        path: "sample/nts-pool/2",
        state: "free",
        task: null,
        ahead: 0,
        behind: 0,
        uncommitted: 0,
        idle_ms: 60000,
        target_bytes: 2e9,
      },
    ],
  };
  const machine = machineReport(
    {
      pool,
      load: "1.2 1.1 0.9",
      measurementLock: { text: "free", color: "green" },
      oomKills: 0,
      units: `Id=nts-full-check.service\nActiveState=inactive\nSubState=dead\nMemoryCurrent=0\nCPUUsageNSec=0\nStateChangeTimestamp=@${Math.floor((now - 3600000) / 1000)}\n\nId=nts-landing-check.service\nActiveState=active\nSubState=running\nMemoryCurrent=104857600\nCPUUsageNSec=1000000000\nStateChangeTimestamp=@${Math.floor((now - 3600000) / 1000)}`,
      ledger: {
        ledger: "sample/memory.json",
        machine: { total_bytes: 32 * gib, available_bytes: 18 * gib, floor_bytes: 12 * gib },
        pending_bytes: 2 * gib,
        room_bytes: 4 * gib,
        admitted: [
          {
            id: "sample/compiler",
            what: "sample compiler build",
            priority: "build",
            bytes: 4 * gib,
            used_bytes: 2 * gib,
            owner_pid: 123,
            since: iso(0.1),
            live: true,
            unmeasured: ["sample-step"],
          },
        ],
        waiting: [
          {
            id: "sample/landing",
            what: "sample landing check",
            priority: "landing",
            bytes: 6 * gib,
            grows: null,
            owner_pid: 456,
            waiter_pid: 457,
            since: iso(0.05),
            waited_s: 180,
            live: true,
          },
        ],
      },
    },
    now,
  );
  const queued = {
    id: "sample-held",
    sha: commit,
    branch: "103-negative-integers",
    issue: 103,
    by: "sample worker",
    submitted_at: iso(0.3),
    note: null,
  };
  const landing = landingReport(
    {
      service: { running: true, state: "idle", since: iso(0.2), message: null },
      checking: null,
      waiting: [],
      held: [{ ...queued, message: "sample memory admission hold" }],
      day: { landed: 1, refused: 1, held: 1, withdrawn: 0, median_wall_s: 540, max_wall_s: 660 },
      main: { commit, pushed: true },
      recent: [
        {
          id: "sample-landed",
          sha: previous,
          branch: "101-separators",
          issue: 101,
          state: "landed",
          at: iso(0.5),
          message: "Sample accepted commit landed and verified",
          run: "sample/run",
          wall_s: 540,
          estimate_s: 500,
          landing: ["build", "regression"],
          deferred: ["census"],
          pending: 2,
        },
      ],
    },
    now,
  );
  return {
    Benchmarks: benchmarkReport(benchmarks, now),
    Conformance: conformanceReport(
      conformance,
      new Map([
        [commit, 22],
        [previous, 140],
        [censusCommit, 12],
      ]),
    ),
    "Full check": full,
    Machine: machine,
    Landing: landing,
  };
}
