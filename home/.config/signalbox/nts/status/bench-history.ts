// The benchmarks over time: each complete official nightly run (results/benchmarks/runs),
// reduced to what progress needs (each case's time and noise per column, and the
// harness's ratio and verdict per nts backend and comparator), read once per run file.
// Progress compares a case only between runs that ran the same workload.
import { readdir, readFile, stat } from "node:fs/promises";
import { join } from "node:path";
import type { Color, Table, TableCell } from "./model.ts";
import { trendText } from "./trend.ts";

export { seriesRange, sparkline, trendText } from "./trend.ts";

export const BACKENDS = [
  ["c", "C", "nts C"],
  ["llvm", "LLVM", "nts LLVM"],
  ["jvm", "JVM", "nts JVM"],
] as const;
export const COMPARATORS = ["node", "Bun JIT", "Bun AOT", "ScriptC"] as const;

/** One case's time in a column: nanoseconds per operation, and its run-to-run spread (×). */
export type Time = { readonly ns: number; readonly spread: number };
export type RunCase = {
  readonly workload: string;
  readonly times: Readonly<Record<string, Time>>;
  /** nts column -> comparator -> the harness's ratio (nts time over theirs) and verdict. */
  readonly versus: Readonly<
    Record<string, Readonly<Record<string, { readonly ratio: number; readonly kind: string }>>>
  >;
};
export type Run = {
  readonly id: string;
  readonly startedAt: string;
  readonly commit: string;
  readonly comparators: Readonly<Record<string, string>>;
  readonly cases: ReadonlyMap<string, RunCase>;
};

type RunFile = {
  schemaVersion?: number;
  id?: string;
  complete?: boolean;
  startedAt?: string;
  compiler?: { commit?: string };
  provenance?: { kind?: string };
  comparators?: Record<string, { version?: string } | null>;
  rows?: {
    name: string;
    workloadIdentity?: string;
    cells?: Record<string, { status?: string; nanoseconds?: number; spread?: number } | null>;
    verdicts?: Record<string, Record<string, { kind?: string; ratio?: number | null } | null>>;
  }[];
};

/** A run file as progress reads it; null when it is not a complete official nightly run. */
export function runOf(file: RunFile): Run | null {
  if (file.schemaVersion !== 1 || file.complete !== true) return null;
  if (file.provenance?.kind !== "official nts-bench" || !Array.isArray(file.rows)) return null;
  const cases = new Map<string, RunCase>();
  for (const row of file.rows) {
    const times: Record<string, Time> = {};
    for (const [column, cell] of Object.entries(row.cells ?? {}))
      if (cell?.status === "OK" && typeof cell.nanoseconds === "number" && cell.nanoseconds > 0)
        times[column] = { ns: cell.nanoseconds, spread: Math.max(1, cell.spread ?? 1) };
    const versus: Record<string, Record<string, { ratio: number; kind: string }>> = {};
    for (const [column, against] of Object.entries(row.verdicts ?? {}))
      for (const [comparator, verdict] of Object.entries(against ?? {}))
        if (typeof verdict?.ratio === "number" && verdict.ratio > 0)
          (versus[column] ??= {})[comparator] = {
            ratio: verdict.ratio,
            kind: verdict.kind ?? "",
          };
    cases.set(row.name, { workload: row.workloadIdentity ?? "", times, versus });
  }
  return {
    id: file.id ?? "",
    startedAt: file.startedAt ?? "",
    commit: file.compiler?.commit ?? "",
    comparators: Object.fromEntries(
      Object.entries(file.comparators ?? {}).map(([name, each]) => [name, each?.version ?? "?"]),
    ),
    cases,
  };
}

/** Each run file's reading, kept while the file is unchanged (runs are written once). */
const read = new Map<string, { readonly mtime: number; readonly run: Run | null }>();

/** The newest complete official runs, oldest first, at most `limit`. */
export async function benchmarkHistory(root: string, limit = 14): Promise<Run[]> {
  const directory = join(root, "results/benchmarks/runs");
  const names = (await readdir(directory).catch(() => [] as string[]))
    .filter((name) => name.endsWith(".json"))
    .sort()
    .reverse();
  const runs: Run[] = [];
  for (const name of names) {
    if (runs.length >= limit) break;
    const path = join(directory, name);
    const { mtimeMs } = await stat(path);
    let known = read.get(path);
    if (known?.mtime !== mtimeMs) {
      const run = runOf(JSON.parse(await readFile(path, "utf8")) as RunFile);
      known = { mtime: mtimeMs, run };
      read.set(path, known);
    }
    if (known.run) runs.push(known.run);
  }
  return runs.reverse();
}

const geomean = (values: readonly number[]) =>
  values.length
    ? Math.exp(values.reduce((sum, value) => sum + Math.log(value), 0) / values.length)
    : NaN;
const percent = (factor: number) =>
  `${factor >= 1 ? "+" : "−"}${Math.abs((factor - 1) * 100).toFixed(1)}%`;
const band = (spread: number) => `±${((spread - 1) * 100).toFixed(1)}%`;
const shortId = (run: Run) => `${run.startedAt.slice(0, 10)} ${run.commit.slice(0, 12)}`;

/** A case ran the same workload in both runs: only then are its times comparable. */
const same = (a: Run, b: Run, name: string) => {
  const left = a.cases.get(name);
  const right = b.cases.get(name);
  return left && right && left.workload === right.workload ? ([left, right] as const) : undefined;
};

/**
 * Against each comparator: the geometric mean of the harness's ratios (nts's time over
 * theirs) across the newest run's cases, then and in each earlier run over the cases it
 * shares with the newest (same workload), so the series compares like with like.
 */
export function progressTable(runs: readonly Run[]): Table | undefined {
  const latest = runs.at(-1);
  if (!latest) return undefined;
  const previous = runs.at(-2);
  const series = (column: string, comparator: string, run: Run) => {
    const ratios: number[] = [];
    for (const name of latest.cases.keys()) {
      const pair = same(run, latest, name);
      const ratio = pair?.[0].versus[column]?.[comparator]?.ratio;
      if (ratio !== undefined && latest.cases.get(name)!.versus[column]?.[comparator])
        ratios.push(ratio);
    }
    return geomean(ratios);
  };
  const rows = BACKENDS.flatMap(([, label, column]) =>
    COMPARATORS.map((comparator) => {
      const now = series(column, comparator, latest);
      const before = previous ? series(column, comparator, previous) : NaN;
      const trend = runs.map((run) => series(column, comparator, run));
      const kinds = [...latest.cases.values()].map(
        (each) => each.versus[column]?.[comparator]?.kind,
      );
      const count = (kind: string) => kinds.filter((each) => each === kind).length;
      const change = now / before;
      // Coloured only beyond the mean's own run-to-run moves (2.5 standard deviations of
      // its earlier changes, from four of them at the fewest).
      const earlier = trend
        .slice(0, -1)
        .flatMap((value, index, all) =>
          index && Number.isFinite(value) && Number.isFinite(all[index - 1]!)
            ? [Math.log(value / all[index - 1]!)]
            : [],
        );
      const mean = earlier.reduce((sum, value) => sum + value, 0) / earlier.length;
      const deviation = Math.sqrt(
        earlier.reduce((sum, value) => sum + (value - mean) ** 2, 0) /
          Math.max(1, earlier.length - 1),
      );
      const beyond =
        earlier.length >= 4 &&
        Number.isFinite(change) &&
        Math.abs(Math.log(change)) > 2.5 * deviation;
      const color: Color | undefined = !beyond ? undefined : change > 1 ? "red" : "green";
      const cell = (text: string, tint?: Color): TableCell => (tint ? { text, color: tint } : text);
      return {
        // The row's identity: one per backend and comparator (selection steps by it).
        pair: `${label} vs ${comparator}`,
        backend: label,
        versus: comparator,
        now: Number.isFinite(now) ? { text: now.toFixed(2), bold: true } : "—",
        before: Number.isFinite(before) ? before.toFixed(2) : "—",
        change: Number.isFinite(change) ? cell(percent(change), color) : "—",
        trend: {
          text: trendText(trend),
          color: "cyan" as const,
          series: trend.map((value) => (Number.isFinite(value) ? value : null)),
        },
        won: cell(String(count("WON")), "green"),
        short: count("SHORT") ? cell(String(count("SHORT")), "yellow") : "0",
        lost: count("LOSS") ? cell(String(count("LOSS")), "red") : "0",
      };
    }),
  );
  return {
    id: "bench-progress",
    title:
      "Against each comparator: the geometric mean of nts's time over theirs (lower is better)",
    key: "pair",
    columns: [
      { key: "backend", label: "nts" },
      { key: "versus", label: "versus" },
      { key: "now", label: "now", align: "right" },
      { key: "before", label: "last run", align: "right" },
      { key: "change", label: "change", align: "right" },
      { key: "trend", label: `last ${runs.length} runs` },
      { key: "won", label: "won", align: "right" },
      { key: "short", label: "short", align: "right" },
      { key: "lost", label: "lost", align: "right" },
    ],
    rows,
    facts: [
      ["newest run", shortId(latest)],
      ...(previous ? ([["compared with", shortId(previous)]] as const) : []),
      ...comparatorChanges(previous, latest),
    ],
  };
}

/** A comparator whose version moved between the two runs: its ratios moved with it. */
function comparatorChanges(previous: Run | undefined, latest: Run): (readonly [string, string])[] {
  if (!previous) return [];
  return Object.entries(latest.comparators)
    .filter(
      ([name, version]) => previous.comparators[name] && previous.comparators[name] !== version,
    )
    .map(
      ([name, version]) =>
        [`${name} changed`, `${previous.comparators[name]} → ${version}`] as const,
    );
}

/** Each nts backend's reference: the comparator built the same way, measured in the same
 * run (native code moves with native code; a JIT's time moves differently). */
export const REFERENCE: Readonly<Record<string, string>> = {
  "nts C": "C++",
  "nts LLVM": "C++",
  "nts JVM": "Java",
};

/**
 * How much faster or slower a reference ran in one run against another: the geometric
 * mean of its time changes over the cases both ran (×; below 1, faster). A night the
 * machine ran faster moves every time; nts's change is net of its own kind's.
 */
export function machineDrift(previous: Run, latest: Run, column: string): number {
  const logs: number[] = [];
  for (const name of latest.cases.keys()) {
    const pair = same(previous, latest, name);
    const before = pair?.[0].times[column];
    const now = pair?.[1].times[column];
    if (before && now) logs.push(Math.log(now.ns / before.ns));
  }
  return logs.length ? Math.exp(logs.reduce((sum, value) => sum + value, 0) / logs.length) : 1;
}

/** A drift per run pair and reference, worked out once. */
const drifts = new Map<string, number>();
const driftOf = (previous: Run, latest: Run, column: string) => {
  const key = `${previous.id}|${latest.id}|${column}`;
  let drift = drifts.get(key);
  if (drift === undefined) {
    drift = machineDrift(previous, latest, REFERENCE[column] ?? column);
    drifts.set(key, drift);
  }
  return drift;
};

/** A case's change between two runs for a backend, net of its reference's drift (×). */
function netChange(previous: Run, latest: Run, name: string, column: string): number | undefined {
  const pair = same(previous, latest, name);
  const before = pair?.[0].times[column];
  const now = pair?.[1].times[column];
  return before && now ? now.ns / before.ns / driftOf(previous, latest, column) : undefined;
}

/** Earlier run pairs a case's noise is learnt from, at the fewest. */
const LEARNT_FROM = 4;
/**
 * A case's noise for a backend (×): how far its net time moved between earlier
 * consecutive runs (2.5 standard deviations of their changes, the pair being judged left
 * out), and never less than the two runs' own spreads. Undefined until enough earlier
 * runs ran it: its changes are not judged before then.
 */
export function caseNoise(runs: readonly Run[], name: string, column: string): number | undefined {
  const logs: number[] = [];
  for (let index = 1; index < runs.length - 1; index++) {
    const change = netChange(runs[index - 1]!, runs[index]!, name, column);
    if (change !== undefined) logs.push(Math.log(change));
  }
  if (logs.length < LEARNT_FROM) return undefined;
  const mean = logs.reduce((sum, value) => sum + value, 0) / logs.length;
  const deviation = Math.sqrt(
    logs.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (logs.length - 1),
  );
  const latest = runs.at(-1)?.cases.get(name)?.times[column];
  const previous = runs.at(-2)?.cases.get(name)?.times[column];
  const spreads = latest && previous ? latest.spread * previous.spread : 1;
  return Math.max(Math.exp(2.5 * deviation), spreads);
}

/** nts's own time per case between the last two runs, net of its reference's drift, when
 * it moved beyond the case's learnt noise. */
export type Change = {
  readonly name: string;
  readonly backend: string;
  readonly now: number;
  readonly before: number;
  /** now over before, over the reference's drift. */
  readonly change: number;
  /** The case's noise (×): a change inside it is not one. */
  readonly noise: number;
};
export function changes(runs: readonly Run[]): Change[] {
  const latest = runs.at(-1);
  const previous = runs.at(-2);
  if (!latest || !previous) return [];
  const found: Change[] = [];
  for (const name of latest.cases.keys()) {
    for (const [, label, column] of BACKENDS) {
      const change = netChange(previous, latest, name, column);
      const noise = change === undefined ? undefined : caseNoise(runs, name, column);
      if (change === undefined || noise === undefined) continue;
      if (change > noise || change < 1 / noise)
        found.push({
          name,
          backend: label,
          now: latest.cases.get(name)!.times[column]!.ns,
          before: previous.cases.get(name)!.times[column]!.ns,
          change,
          noise,
        });
    }
  }
  // Regressions first, the largest first; then improvements, the largest first.
  return found.sort((x, y) =>
    x.change > 1 !== y.change > 1
      ? x.change > 1
        ? -1
        : 1
      : x.change > 1
        ? y.change - x.change
        : x.change - y.change,
  );
}

const nanoseconds = (ns: number) =>
  ns >= 1e6
    ? `${(ns / 1e6).toFixed(2)} ms`
    : ns >= 1e3
      ? `${(ns / 1e3).toFixed(2)} µs`
      : `${ns.toFixed(1)} ns`;

export function changesTable(runs: readonly Run[]): Table | undefined {
  const latest = runs.at(-1);
  const previous = runs.at(-2);
  if (!latest || !previous) return undefined;
  const found = changes(runs);
  const regressions = found.filter((each) => each.change > 1).length;
  return {
    id: "bench-changes",
    title: `Since the last run, beyond each case's noise: ${regressions} slower, ${found.length - regressions} faster`,
    key: "case",
    columns: [
      { key: "case", label: "case" },
      { key: "backend", label: "nts" },
      { key: "change", label: "change", align: "right" },
      { key: "noise", label: "noise", align: "right" },
      { key: "now", label: "now", align: "right" },
      { key: "before", label: "last run", align: "right" },
      { key: "node", label: "/node now", align: "right" },
    ],
    rows: found.map((each) => {
      const column = BACKENDS.find(([, label]) => label === each.backend)![2];
      const ratio = latest.cases.get(each.name)?.versus[column]?.node?.ratio;
      return {
        case: `${each.name} (${each.backend})`,
        backend: each.backend,
        change: {
          text: percent(each.change),
          color: each.change > 1 ? ("red" as const) : ("green" as const),
          ...(each.change > 1.1 ? { bold: true } : {}),
        },
        noise: band(each.noise),
        now: nanoseconds(each.now),
        before: nanoseconds(each.before),
        node: ratio === undefined ? "—" : ratio.toFixed(2),
      };
    }),
    details: Object.fromEntries(
      found.flatMap((each) => {
        const history = caseHistoryTable(runs, each.name);
        return history ? [[`${each.name} (${each.backend})`, history]] : [];
      }),
    ),
    facts: [
      ["compared", `${shortId(previous)} → ${shortId(latest)}`],
      [
        "machine",
        `C++ ran ${percent(machineDrift(previous, latest, "C++"))} and Java ${percent(machineDrift(previous, latest, "Java"))} between the runs: C and LLVM changes are net of C++'s, JVM's of Java's`,
      ],
      [
        "noise",
        "each case's own: how far it moved between earlier runs (2.5 standard deviations), at least its spreads; a change inside it is not listed",
      ],
    ],
  };
}

/** One case's C/node over the runs, and C's change since the last run with its noise (the
 * same measure as the changes table: net of C++'s drift, against the case's learnt noise). */
export function caseProgress(runs: readonly Run[], name: string) {
  const latest = runs.at(-1);
  const previous = runs.at(-2);
  if (!latest?.cases.has(name)) return undefined;
  const ratio = (run: Run) => same(run, latest, name)?.[0].versus["nts C"]?.node?.ratio;
  const noise = caseNoise(runs, name, "nts C");
  const factor = previous ? netChange(previous, latest, name, "nts C") : undefined;
  return {
    band: noise === undefined ? undefined : band(noise),
    change:
      factor === undefined
        ? undefined
        : {
            factor,
            beyondNoise: noise !== undefined && (factor > noise || factor < 1 / noise),
          },
    trend: trendText(runs.map(ratio)),
    series: runs.map((run) => ratio(run) ?? null),
  };
}

/** How many cases got slower beyond their noise since the last run, per the newest two runs. */
export function regressionCount(runs: readonly Run[]): number | undefined {
  const latest = runs.at(-1);
  const previous = runs.at(-2);
  if (!latest || !previous) return undefined;
  return changes(runs).filter((each) => each.change > 1).length;
}

/**
 * One case's runs, newest first: the real times and the harness's ratios each run
 * recorded. A run whose workload differs from the newest says so: its numbers are of
 * another program.
 */
export function caseHistoryTable(runs: readonly Run[], name: string): Table | undefined {
  const latest = runs.at(-1)?.cases.get(name);
  if (!latest) return undefined;
  const ratio = (each: RunCase, column: string, comparator: string): TableCell => {
    const value = each.versus[column]?.[comparator]?.ratio;
    return value === undefined ? "—" : value.toFixed(2);
  };
  const time = (each: RunCase, column: string): TableCell => {
    const value = each.times[column];
    return value ? `${nanoseconds(value.ns)} ${band(value.spread)}` : "—";
  };
  const rows = [...runs].reverse().flatMap((run) => {
    const each = run.cases.get(name);
    if (!each) return [];
    const other = each.workload !== latest.workload;
    return [
      {
        run: other
          ? { text: `${run.startedAt.slice(0, 10)} (another workload)`, color: "gray" as const }
          : run.startedAt.slice(0, 10),
        commit: run.commit.slice(0, 12),
        c: time(each, "nts C"),
        node: time(each, "node"),
        cNode: ratio(each, "nts C", "node"),
        llvmNode: ratio(each, "nts LLVM", "node"),
        jvmNode: ratio(each, "nts JVM", "node"),
        cAot: ratio(each, "nts C", "Bun AOT"),
        cScriptc: ratio(each, "nts C", "ScriptC"),
        cpp: time(each, "C++"),
      },
    ];
  });
  return {
    id: `bench-case/${name}`,
    title: `${name}: its last ${rows.length} runs, newest first`,
    key: "run",
    columns: [
      { key: "run", label: "run" },
      { key: "commit", label: "compiler" },
      { key: "c", label: "C time", align: "right" },
      { key: "node", label: "node time", align: "right" },
      { key: "cNode", label: "C/node", align: "right" },
      { key: "llvmNode", label: "LLVM/node", align: "right" },
      { key: "jvmNode", label: "JVM/node", align: "right" },
      { key: "cAot", label: "C/Bun AOT", align: "right" },
      { key: "cScriptc", label: "C/ScriptC", align: "right" },
      { key: "cpp", label: "C++ time", align: "right" },
    ],
    rows,
    facts: [["times", "nanoseconds per operation, with each run's own spread"]],
  };
}
