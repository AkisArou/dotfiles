// What nts measured, for signalbox's Benchmarks and Conformance tabs, read
// from `node tooling/records/summary.ts benchmarks|conformance` (version 1):
// the newest complete benchmark run held to the bar against node, Bun's JIT,
// Bun's ahead-of-time compiler and ScriptC, and the records each backend is
// held to. Each tab says first how old what it shows is: a run's date and
// the node it ran against, a record's age in compiler commits. Read every
// five minutes; nothing here watches a file.

import type { Gauge, Provider, ProviderContext, Table, TableCell } from "signalbox";
import { checked } from "./status.ts";

type Backend = "c" | "llvm" | "jvm";
const BACKENDS: readonly Backend[] = ["c", "llvm", "jvm"];
const NAME: Readonly<Record<Backend, string>> = { c: "C", llvm: "LLVM", jvm: "JVM" };
const COMPARATORS = ["node", "Bun JIT", "Bun AOT", "ScriptC"] as const;
type Comparator = (typeof COMPARATORS)[number];
type Kind = "WON" | "SHORT" | "LOSS" | "TIE" | "UNAVAILABLE" | "NOT_COMPARABLE";

interface Verdict {
  readonly kind: Kind;
  readonly ratio: number | null;
  readonly tolerance: number | null;
}

/** What `summary.ts benchmarks` prints. */
export interface BenchmarksSummary {
  readonly run: {
    readonly id: string;
    readonly startedAt: string;
    readonly finishedAt: string;
    readonly compiler: { readonly commit: string };
    readonly machine: { readonly conditions: { readonly cpu?: string; readonly sibling?: number; readonly start?: { readonly load?: string; readonly memoryAvailableBytes?: number } } };
    readonly comparators: Readonly<Record<string, { readonly version: string; readonly commit: string | null } | null>>;
  } | null;
  readonly counts: Readonly<Record<Backend, Readonly<Record<Comparator, Readonly<Partial<Record<Kind, number>>>>>>> | null;
  readonly rows: readonly {
    readonly name: string;
    readonly backends: Readonly<Record<Backend, { readonly versus: Readonly<Record<Comparator, Verdict>>; readonly slowerThanBest: boolean }>>;
  }[];
}

interface Outcomes {
  readonly cases?: number;
  readonly outcomes?: Readonly<Record<string, number>>;
  readonly pending?: number;
  readonly commit?: string | null;
}

/** What `summary.ts conformance` prints. */
export interface ConformanceSummary {
  readonly backends: Readonly<
    Record<
      Backend,
      {
        readonly test262?: Outcomes & { readonly slices: Readonly<Record<string, Outcomes & { readonly under: string }>> };
        readonly typescript?: Outcomes;
        readonly benchmarks?: Outcomes & { readonly scopes: Readonly<Record<string, Outcomes>> };
        readonly benchmarkRuns?: Outcomes;
        readonly dex?: Outcomes;
      }
    >
  >;
}

export const BENCHMARKS = "nts-benchmarks";
export const CONFORMANCE = "nts-conformance";
export const BENCHMARK_TABLES = ["bench-run", "bench-counts", "bench-rows"];
export const CONFORMANCE_TABLES = ["conformance"];

const DAY = 86_400_000;
const short = (commit: string | null | undefined) => commit?.slice(0, 12) ?? "?";
const days = (ms: number) => (ms < DAY ? `${Math.round(ms / 3_600_000)} h` : `${(ms / DAY).toFixed(1)} days`);
const VERDICT_COLOR: Readonly<Partial<Record<Kind, "green" | "yellow" | "red" | "gray">>> = { WON: "green", SHORT: "yellow", LOSS: "red", UNAVAILABLE: "gray", NOT_COMPARABLE: "gray" };

/** A ratio with its verdict's color: below 1 is faster than the comparator. */
function ratioCell(verdict: Verdict | undefined): TableCell {
  if (!verdict || verdict.ratio === null) return { text: verdict?.kind === "NOT_COMPARABLE" ? "n/c" : "—", color: "gray" };
  const color = VERDICT_COLOR[verdict.kind];
  return { text: verdict.ratio.toFixed(2), ...(color ? { color } : {}), ...(verdict.kind === "LOSS" ? { bold: true } : {}) };
}

/** The run's identity and conditions: what the numbers are of, and how old they are. */
function runTable(summary: BenchmarksSummary, now: number): Table {
  const run = summary.run;
  if (!run) return { id: "bench-run", title: "No complete benchmark run is kept", columns: [{ key: "what", label: "" }, { key: "state", label: "" }], rows: [] };
  const age = now - Date.parse(run.finishedAt);
  const conditions = run.machine.conditions;
  const comparator = (name: string) => run.comparators[name]?.version ?? "not run";
  return {
    id: "bench-run",
    title: "The newest complete run",
    columns: [{ key: "what", label: "" }, { key: "state", label: "" }],
    rows: [
      { what: "finished", state: { text: `${run.finishedAt.slice(0, 16).replace("T", " ")} UTC, ${days(age)} ago`, ...(age > 7 * DAY ? { color: "red" as const } : age > 2 * DAY ? { color: "yellow" as const } : {}) } },
      { what: "compiler", state: short(run.compiler.commit) },
      { what: "against", state: COMPARATORS.map((name) => `${name} ${comparator(name)}`).join(" · ") },
      { what: "conditions", state: `CPU ${conditions.cpu ?? "?"} with sibling ${conditions.sibling ?? "?"} idle; load at the start ${conditions.start?.load?.split(" ").slice(0, 3).join(" ") ?? "?"}` },
    ],
    at: Date.parse(run.finishedAt),
  };
}

/** Each backend against each comparator: how many rows it won, tied, fell short on, lost. */
function countsTable(summary: BenchmarksSummary): Table {
  const counts = summary.counts;
  const number = (value: number | undefined, color?: "red" | "green"): TableCell => (value ? (color ? { text: String(value), color } : value) : "");
  return {
    id: "bench-counts",
    title: "Against the comparators: rows won, tied, short (not 0.8× node or Bun JIT), lost",
    columns: [
      { key: "backend", label: "backend" },
      { key: "versus", label: "against" },
      { key: "won", label: "won", align: "right" },
      { key: "tie", label: "tie", align: "right" },
      { key: "short", label: "short", align: "right" },
      { key: "lost", label: "lost", align: "right" },
      { key: "none", label: "no time", align: "right" },
    ],
    rows: counts
      ? BACKENDS.flatMap((backend) =>
          COMPARATORS.map((versus) => {
            const kinds = counts[backend]?.[versus] ?? {};
            return {
              backend: NAME[backend],
              versus,
              won: number(kinds.WON, "green"),
              tie: number(kinds.TIE),
              short: number(kinds.SHORT),
              lost: number(kinds.LOSS, "red"),
              none: number((kinds.UNAVAILABLE ?? 0) + (kinds.NOT_COMPARABLE ?? 0)),
            };
          }),
        )
      : [],
  };
}

/** Every case, worst first: each backend's ratio to node, C's to Bun AOT and ScriptC, and which are slower than their own best. */
function rowsTable(summary: BenchmarksSummary): Table {
  const worst = (row: BenchmarksSummary["rows"][number]) => Math.max(...BACKENDS.map((backend) => row.backends[backend]?.versus.node?.ratio ?? 0));
  return {
    id: "bench-rows",
    title: "Each case, worst against node first: a backend's time over the comparator's",
    key: "case",
    columns: [
      { key: "case", label: "case" },
      ...BACKENDS.map((backend) => ({ key: backend, label: `${NAME[backend]}/node`, align: "right" as const })),
      { key: "aot", label: "C/Bun AOT", align: "right" },
      { key: "scriptc", label: "C/ScriptC", align: "right" },
      { key: "slower", label: "slower than its best" },
    ],
    rows: [...summary.rows]
      .sort((a, b) => worst(b) - worst(a))
      .map((row) => {
        const slower = BACKENDS.filter((backend) => row.backends[backend]?.slowerThanBest).map((backend) => NAME[backend]);
        return {
          case: row.name,
          ...Object.fromEntries(BACKENDS.map((backend) => [backend, ratioCell(row.backends[backend]?.versus.node)])),
          aot: ratioCell(row.backends.c?.versus["Bun AOT"]),
          scriptc: ratioCell(row.backends.c?.versus.ScriptC),
          slower: slower.length ? { text: slower.join(", "), color: "yellow" } : "",
        };
      }),
  };
}

/**
 * The benchmarks' gauges: how old the newest complete run is (the full check
 * runs them nightly, so two days wants a look and a week is late), the rows
 * slower than their own best, and the rows lost to node.
 */
export function benchmarkGauges(summary: BenchmarksSummary, now: number): Gauge[] {
  const section = "benchmarks";
  const run = summary.run;
  if (!run) return [{ id: "run", label: "the newest complete run", section, value: 0, text: "none kept", state: "red", why: "results/benchmarks/runs/ holds no complete run" }];
  const slower = summary.rows.flatMap((row) => BACKENDS.filter((backend) => row.backends[backend]?.slowerThanBest).map((backend) => `${row.name} (${NAME[backend]})`));
  const lost = BACKENDS.map((backend) => [backend, summary.counts?.[backend]?.node?.LOSS ?? 0] as const);
  return [
    {
      id: "run",
      label: "the newest complete run",
      section,
      value: Math.round(((now - Date.parse(run.finishedAt)) / DAY) * 10) / 10,
      unit: "days",
      limits: { amber: 2, red: 7 },
      why: `run ${run.id}, against node ${run.comparators["node"]?.version ?? "?"}: every number on the tab is from it`,
    },
    { id: "slower", label: "rows slower than their own best", section, value: slower.length, limits: { amber: 1 }, ...(slower.length ? { why: slower.join(", ") } : {}) },
    { id: "lost", label: "rows lost to node", section, value: lost.reduce((sum, [, count]) => sum + count, 0), text: lost.map(([backend, count]) => `${NAME[backend]} ${count}`).join(", "), why: "the goal is to beat node on every row" },
  ];
}

/** What a record's outcomes other than passing come to: `wrong 390 · runtime-abort 224`. */
const failing = (outcomes: Readonly<Record<string, number>> | undefined) =>
  Object.entries(outcomes ?? {})
    .filter(([kind]) => kind !== "pass")
    .sort(([, a], [, b]) => b - a)
    .map(([kind, count]) => `${kind} ${count}`)
    .join(" · ");

const total = (outcomes: Readonly<Record<string, number>> | undefined) => Object.values(outcomes ?? {}).reduce((sum, count) => sum + count, 0);

/** The records each backend is held to, a row each: how many pass, what the rest are, what is pending, and how old the record is. */
function conformanceTable(summary: ConformanceSummary, behind: ReadonlyMap<string, number>): Table {
  const age = (commits: readonly (string | null | undefined)[]): TableCell => {
    const known = [...new Set(commits.filter((commit): commit is string => Boolean(commit)))];
    if (!known.length) return "";
    const oldest = Math.max(...known.map((commit) => behind.get(commit) ?? 0));
    return { text: `${oldest} compiler commits ago${known.length > 1 ? ` (oldest of ${known.length})` : ""}`, ...(oldest >= 100 ? { color: "yellow" as const } : {}) };
  };
  const rows = BACKENDS.flatMap((backend) => {
    const records = summary.backends[backend];
    if (!records) return [];
    const row = (record: string, outcomes: Readonly<Record<string, number>> | undefined, pending: number | undefined, commits: readonly (string | null | undefined)[]) => {
      const cases = total(outcomes);
      const pass = outcomes?.["pass"] ?? 0;
      return {
        record: `${NAME[backend]} ${record}`,
        pass: cases ? `${pass} (${((100 * pass) / cases).toFixed(1)}%)` : "",
        cases,
        rest: failing(outcomes),
        pending: pending ? { text: String(pending), color: "yellow" as const } : "",
        written: age(commits),
      };
    };
    return [
      ...(records.test262 ? [row("test262", records.test262.outcomes, undefined, Object.values(records.test262.slices).map((slice) => slice.commit))] : []),
      ...(records.typescript ? [row("TypeScript", records.typescript.outcomes, records.typescript.pending, [records.typescript.commit])] : []),
      ...(records.benchmarks ? Object.entries(records.benchmarks.scopes).map(([scope, outcomes]) => row(`benchmarks ${scope}`, outcomes.outcomes, records.benchmarks?.pending, [records.benchmarks?.commit])) : []),
      ...(records.benchmarkRuns ? [row("benchmark runs", records.benchmarkRuns.outcomes, records.benchmarkRuns.pending, [records.benchmarkRuns.commit])] : []),
      ...(records.dex ? [row("dex", records.dex.outcomes, records.dex.pending, [records.dex.commit])] : []),
    ];
  });
  return {
    id: "conformance",
    title: "The records each backend is held to; pending cases are written by the full check after landing",
    key: "record",
    columns: [
      { key: "record", label: "record" },
      { key: "pass", label: "pass", align: "right" },
      { key: "cases", label: "of", align: "right" },
      { key: "pending", label: "pending", align: "right" },
      { key: "written", label: "written" },
      { key: "rest", label: "the rest" },
    ],
    rows,
  };
}

/** The conformance gauges: how many compiler commits old the test262 records are, since they understate what passes once main moves on. */
export function conformanceGauges(summary: ConformanceSummary, behind: ReadonlyMap<string, number>): Gauge[] {
  const commits = BACKENDS.flatMap((backend) => Object.values(summary.backends[backend]?.test262?.slices ?? {}).map((slice) => slice.commit)).filter((commit): commit is string => Boolean(commit));
  if (!commits.length) return [];
  const oldest = Math.max(...commits.map((commit) => behind.get(commit) ?? 0));
  return [
    {
      id: "test262.age",
      label: "the test262 records' age",
      section: "conformance",
      value: oldest,
      unit: "compiler commits",
      limits: { amber: 100 },
      why: "commits to compiler/ and runtime/ since they were written: they understate what passes today",
    },
  ];
}

/** How many commits to compiler/ and runtime/ main has made since each commit. */
async function compilerCommitsSince(context: ProviderContext, commits: readonly string[]): Promise<Map<string, number>> {
  const counted = await Promise.all(
    [...new Set(commits)].map(async (commit) => [commit, Number((await context.run(["git", "rev-list", "--count", `${commit}..main`, "--", "compiler", "runtime"], { cwd: context.roots[0], timeoutMs: 30_000 })).trim())] as const),
  );
  return new Map(counted);
}

const SUMMARY = (kind: string) => ["node", "tooling/records/summary.ts", kind];

/** The Benchmarks tab's tables, read every five minutes. */
export function ntsBenchmarks(): Provider {
  return {
    id: BENCHMARKS,
    label: "Benchmarks",
    intervalMs: 300_000,
    read: async (context) => {
      const summary = checked<BenchmarksSummary>(JSON.parse(await context.run(SUMMARY("benchmarks"), { cwd: context.roots[0], timeoutMs: 60_000 })), "benchmarks", [1], "benchmarks");
      const now = Date.now();
      return { tables: [runTable(summary, now), countsTable(summary), rowsTable(summary)], gauges: benchmarkGauges(summary, now) };
    },
  };
}

/** The Conformance tab's table, read every five minutes. */
export function ntsConformance(): Provider {
  return {
    id: CONFORMANCE,
    label: "Conformance",
    intervalMs: 300_000,
    read: async (context) => {
      const summary = checked<ConformanceSummary>(JSON.parse(await context.run(SUMMARY("conformance"), { cwd: context.roots[0], timeoutMs: 60_000 })), "conformance", [1], "conformance");
      const commits = BACKENDS.flatMap((backend) => {
        const records = summary.backends[backend];
        return [
          ...Object.values(records?.test262?.slices ?? {}).map((slice) => slice.commit),
          records?.typescript?.commit,
          records?.benchmarks?.commit,
          records?.benchmarkRuns?.commit,
          records?.dex?.commit,
        ];
      }).filter((commit): commit is string => Boolean(commit));
      const behind = await compilerCommitsSince(context, commits);
      return { tables: [conformanceTable(summary, behind)], gauges: conformanceGauges(summary, behind) };
    },
  };
}
