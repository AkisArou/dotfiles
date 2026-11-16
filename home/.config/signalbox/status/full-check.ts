// NTS full-check projections, ported from ~/.config/signalbox/nts/full-check.ts.
import type { Gauge, Report, Table, TableCell, TableRow } from "./model.ts";
import { checked } from "./status.ts";

interface RunRecord {
  readonly commit: string;
  readonly run: string;
  readonly verdict: "green" | "red" | "held";
  readonly at: string;
  readonly failed: readonly string[];
  readonly reason?: string;
  /** A green run's pending cases, and the changes it accepted (improvements, and refusals for other reasons). */
  readonly listed?: { readonly pending: number; readonly accepted: number } | null;
}

/** A part's last records run: what it recorded, when, and what came of it. */
interface RecordsCommit {
  readonly commit: string;
  readonly at: string;
  readonly submitted: string | null;
  readonly result: string;
  /** The green run whose checks listed the rows. */
  readonly run?: string;
  /** The records unit that records them, while it runs and after. */
  readonly job?: {
    readonly unit: string;
    readonly outcome: string;
    readonly at: string;
    readonly starts: number;
    readonly ended?: string;
  };
  /** How many times its rows were applied on main. */
  readonly attempts?: number;
  /** When the full check stopped following it: landed, withdrawn, nothing to apply, or given up. */
  readonly settled?: string;
  /** Where it stands, kept at each step (with #604; absent from an older controller). */
  readonly outcome?:
    | "recording"
    | "submitted"
    | "failed"
    | "landed"
    | "nothing"
    | "given-up"
    | "withdrawn";
}

/** The last night's benchmarks: what they measured, and then their results commit's way through the landing. */
interface BenchmarksRun {
  readonly commit: string;
  readonly at: string;
  readonly result: string;
  /** Whether the run kept a record. */
  readonly measured: boolean;
  /** How the night ended (`record`, `no-record`, `interrupted`, `not-run`), then, with #604, where its results commit stands. */
  readonly outcome?:
    | "record"
    | "no-record"
    | "interrupted"
    | "not-run"
    | "submitted"
    | "failed"
    | "landed"
    | "nothing"
    | "given-up"
    | "withdrawn";
}

interface PartStatus {
  readonly last_run: RunRecord | null;
  readonly last_green: RunRecord | null;
  /** Each part records its own kinds, after its own green run. */
  readonly records: {
    readonly listed: { readonly pending: number; readonly accepted: number } | null;
    readonly last: RecordsCommit | null;
  };
}

export interface FullCheckStatus {
  readonly controller: { readonly running: boolean };
  readonly now: {
    readonly doing: string;
    readonly commit: string;
    readonly since: string;
    readonly seconds: number;
  } | null;
  readonly main: string | null;
  readonly verified: { readonly commit: string | null; readonly pushed: string | null };
  readonly behind: {
    readonly commits: number;
    readonly oldest_landed_at: string | null;
    readonly delay_s: number | null;
  } | null;
  readonly core: PartStatus;
  readonly wide: PartStatus & { readonly slot: string | null };
  readonly culprits: readonly {
    readonly commit: string;
    readonly part: string;
    readonly steps: readonly string[];
    readonly run: string;
    readonly found_at: string;
    readonly revert: string | null;
    readonly reason?: string;
  }[];
  readonly benchmarks: {
    readonly last: BenchmarksRun | null;
    readonly slower: readonly string[];
    readonly due: { readonly since: string; readonly until: string } | null;
  };
  readonly count: {
    readonly at: string;
    readonly verdict: string;
    readonly lines: readonly string[];
  } | null;
  readonly tasks: readonly { readonly at: string; readonly line: string }[];
}

export const FULL_CHECK = "nts-full-check";
/** The Full check tab's tables, in its order. */
export const FULL_CHECK_TABLES = [
  "full-check-state",
  "full-check-parts",
  "full-check-culprits",
  "full-check-records",
  "full-check-count",
  "full-check-tasks",
];

const short = (commit: string | null | undefined) => commit?.slice(0, 12) ?? "?";
const hours = (seconds: number) =>
  seconds < 3600 ? `${Math.round(seconds / 60)} min` : `${(seconds / 3600).toFixed(1)} h`;
const since = (iso: string, now: number) => hours(Math.max(0, (now - Date.parse(iso)) / 1000));
const VERDICT: Record<RunRecord["verdict"], TableCell> = {
  green: { text: "green", color: "green" },
  red: { text: "red", color: "red", bold: true },
  held: { text: "held", color: "yellow" },
};
const two = (
  id: string,
  title: string,
  rows: readonly (readonly [string, TableCell])[],
): Table => ({
  id,
  title,
  columns: [
    { key: "what", label: "" },
    { key: "state", label: "" },
  ],
  rows: rows.map(([what, state]) => ({ what, state })),
});

function benchmarksCell(bench: BenchmarksRun, now: number): TableCell {
  const color = RECORDS_COLOR[benchmarksPhase(bench)];
  return {
    text: `${short(bench.commit)}, ${since(bench.at, now)} ago: ${bench.result}`,
    ...(color ? { color } : {}),
  };
}

/** The full check's status as a report: its tables, the core's and wide's runs, a red count as a decision. */
export function fullCheckReport(status: FullCheckStatus, now = Date.now()): Report {
  const delay = status.behind?.delay_s ?? null;
  // Landing to verified: past an hour wants a look, past three it is late.
  const behind: TableCell = status.behind
    ? {
        text: `${status.behind.commits} commits${delay !== null ? `, the oldest landed ${hours(delay)} ago` : ""}`,
        ...(delay !== null && delay > 3 * 3600
          ? { color: "red" as const }
          : delay !== null && delay > 3600
            ? { color: "yellow" as const }
            : {}),
      }
    : "nothing verified yet";
  const state = two("full-check-state", "Full check", [
    [
      "controller",
      status.controller.running
        ? { text: "running", color: "green" }
        : { text: "not running", color: "red", bold: true },
    ],
    [
      "now",
      status.now
        ? `${status.now.doing} on ${short(status.now.commit)}, for ${hours(status.now.seconds)}`
        : "idle",
    ],
    ["main", short(status.main)],
    [
      "verified",
      `${short(status.verified.commit)}${status.verified.pushed === status.verified.commit ? ", pushed" : status.verified.pushed ? `, pushed ${short(status.verified.pushed)}` : ""}`,
    ],
    ["behind", behind],
    [
      "benchmarks",
      status.benchmarks.last ? benchmarksCell(status.benchmarks.last, now) : "none run yet",
    ],
    ...(status.benchmarks.due
      ? ([
          [
            "benchmarks due",
            `${status.benchmarks.due.since.slice(11, 16)} to ${status.benchmarks.due.until.slice(11, 16)}`,
          ],
        ] as const)
      : []),
    [
      "count",
      status.count
        ? status.count.verdict === "PASS"
          ? { text: "PASS", color: "green" }
          : { text: `${status.count.verdict}: ${status.count.lines.length} lines`, color: "red" }
        : "not run yet",
    ],
  ]);
  const part = (name: "core" | "wide", status: PartStatus): TableRow => {
    const last = status.last_run;
    return {
      part: name,
      verdict: last ? VERDICT[last.verdict] : "none yet",
      commit: short(last?.commit),
      when: last ? `${since(last.at, now)} ago` : "",
      failed: last?.failed.length ? { text: last.failed.join(", "), color: "red" } : "",
      green:
        last?.verdict === "green"
          ? "this one"
          : status.last_green
            ? `${short(status.last_green.commit)}, ${since(status.last_green.at, now)} ago`
            : "none",
      run: last?.run ?? "",
    };
  };
  const parts: Table = {
    id: "full-check-parts",
    title: "Core and wide",
    columns: [
      { key: "part", label: "part" },
      { key: "verdict", label: "verdict" },
      { key: "commit", label: "commit" },
      { key: "when", label: "when", align: "right" },
      { key: "failed", label: "failed" },
      { key: "green", label: "last green" },
      { key: "run", label: "run" },
    ],
    rows: [part("core", status.core), part("wide", status.wide)],
  };
  const culprits: Table = {
    id: "full-check-culprits",
    title: "Culprits",
    key: "commit",
    columns: [
      { key: "commit", label: "commit" },
      { key: "part", label: "part" },
      { key: "steps", label: "steps" },
      { key: "revert", label: "revert" },
      { key: "found", label: "found", align: "right" },
      { key: "reason", label: "why" },
    ],
    rows: status.culprits.map((culprit) => ({
      commit: { text: short(culprit.commit), color: "red" },
      part: culprit.part,
      steps: culprit.steps.join(", "),
      revert: culprit.revert
        ? short(culprit.revert)
        : { text: "none could be made", color: "yellow" },
      found: `${since(culprit.found_at, now)} ago`,
      reason: culprit.reason ?? "",
    })),
  };
  // One row per part: what its last green run listed, and its last records commit.
  const records: Table = {
    id: "full-check-records",
    title: "Records",
    columns: [
      { key: "part", label: "part" },
      { key: "pending", label: "pending", align: "right" },
      { key: "accepted", label: "accepted", align: "right" },
      { key: "last", label: "last records commit" },
    ],
    rows: (["core", "wide"] as const).map((name) => {
      const { listed, last } = status[name].records;
      return {
        part: name,
        pending: listed?.pending ?? "",
        accepted: listed?.accepted ?? "",
        last: last
          ? {
              text: `${short(last.commit)}, ${since(last.at, now)} ago: ${last.result}`,
              ...(RECORDS_COLOR[recordsPhase(last)]
                ? { color: RECORDS_COLOR[recordsPhase(last)] }
                : {}),
            }
          : "none yet",
      };
    }),
  };
  const count: Table = {
    id: "full-check-count",
    title: status.count
      ? `The count: ${status.count.verdict}, ${since(status.count.at, now)} ago`
      : "The count",
    columns: [{ key: "line", label: "line" }],
    rows:
      status.count?.verdict === "PASS"
        ? []
        : (status.count?.lines ?? []).map((line) => ({ line: { text: line, color: "red" } })),
  };
  const tasks: Table = {
    id: "full-check-tasks",
    title: "Still to be made issues",
    columns: [
      { key: "line", label: "line" },
      { key: "when", label: "noted", align: "right" },
    ],
    rows: status.tasks.map((task) => ({
      line: task.line.replace(/^- /, ""),
      when: `${since(task.at, now)} ago`,
    })),
  };
  return {
    tables: [state, parts, culprits, records, count, tasks],
    gauges: fullCheckGauges(status, now),
  };
}

/**
 * Where a part's last records run stands: under way (recording, or submitted
 * to the landing check), done (landed, or nothing to land), failing now, or
 * given up or withdrawn without landing (its rows stay pending for the next
 * records run).
 */
type RecordsPhase = "under way" | "done" | "failed" | "not landed";
const PHASE: Record<NonNullable<RecordsCommit["outcome"]>, RecordsPhase> = {
  recording: "under way",
  submitted: "under way",
  failed: "failed",
  landed: "done",
  nothing: "done",
  "given-up": "not landed",
  withdrawn: "not landed",
};
function recordsPhase(last: RecordsCommit): RecordsPhase {
  if (last.outcome) return PHASE[last.outcome] ?? "under way";
  // A controller older than #604 says it only in its result.
  if (
    /^(failed|record-pending\.ts --apply exited|wrote \d+ record file)/.test(last.result) ||
    / did not start: /.test(last.result)
  )
    return "failed";
  if (/^(landed as|nothing left to commit|recorded nothing)/.test(last.result)) return "done";
  return last.settled ? "not landed" : "under way";
}
const RECORDS_COLOR: Partial<Record<RecordsPhase | "unmeasured", "red" | "yellow">> = {
  failed: "red",
  "not landed": "yellow",
  unmeasured: "yellow",
};

/** Where the last night's benchmarks stand: their results commit's phase, as a records run's, or a night that kept no record. */
function benchmarksPhase(bench: BenchmarksRun): RecordsPhase | "unmeasured" {
  const outcome = bench.outcome;
  if (outcome && outcome in PHASE) return PHASE[outcome as keyof typeof PHASE];
  return bench.measured ? "under way" : "unmeasured";
}

/**
 * The full check's gauges: landing to verified (amber past an hour, red past
 * three), the controller, the core's and the wide steps' last verdicts, the
 * culprits whose reverts have not landed, the records, and the count.
 */
export function fullCheckGauges(status: FullCheckStatus, now = Date.now()): Gauge[] {
  const section = "full check";
  const delay = status.behind?.delay_s ?? null;
  const verdict = (part: "core" | "wide"): Gauge => {
    const last = status[part].last_run;
    const red = part === "core" ? "red" : "amber";
    return {
      id: part,
      label: part === "core" ? "the core's last run" : "the wide steps' last run",
      section,
      value: last?.verdict === "red" ? 1 : 0,
      text: last
        ? `${last.verdict} on ${short(last.commit)}, ${since(last.at, now)} ago`
        : "none yet",
      state: last?.verdict === "red" ? red : last?.verdict === "held" ? "amber" : "ok",
      ...(last?.failed.length
        ? { why: `failed: ${last.failed.join(", ")}; log ${last.run}` }
        : last?.reason
          ? { why: last.reason }
          : {}),
    };
  };
  // A part's pending cases wait for its records commit; one listed and unwritten for a day is late.
  const records = (["core", "wide"] as const).map((part): Gauge => {
    const { listed, last } = status[part].records;
    const pending = listed?.pending ?? 0;
    const lastAt = last ? Date.parse(last.at) : undefined;
    const late = pending > 0 && (lastAt === undefined || now - lastAt > 86_400_000);
    const phase = last ? recordsPhase(last) : undefined;
    return {
      id: `records.${part}`,
      label: `${part} records`,
      section,
      value: pending,
      text: `${pending} pending${last ? `; last written ${since(last.at, now)} ago` : ""}`,
      state: phase === "failed" ? "red" : phase === "not landed" || late ? "amber" : "ok",
      ...(phase === "failed" || phase === "not landed"
        ? { why: last?.result }
        : late
          ? { why: "listed and not written within a day" }
          : {}),
    };
  });
  return [
    {
      id: "controller",
      label: "the full check",
      section,
      value: status.controller.running ? 1 : 0,
      text: status.controller.running
        ? status.now
          ? `${status.now.doing} on ${short(status.now.commit)}, for ${hours(status.now.seconds)}`
          : "idle"
        : "not running",
      state: status.controller.running ? "ok" : "red",
      ...(status.controller.running
        ? {}
        : { why: "systemctl --user status nts-full-check says why" }),
    },
    {
      id: "behind",
      label: "landing to verified",
      section,
      value: delay === null ? 0 : Math.round((delay / 3600) * 10) / 10,
      unit: "hours",
      ...(delay === null ? { text: "nothing waits" } : {}),
      limits: { amber: 1, red: 3 },
      ...(status.behind?.commits
        ? { why: `${status.behind.commits} commits on main not verified yet` }
        : {}),
    },
    verdict("core"),
    verdict("wide"),
    {
      id: "culprits",
      label: "culprits whose revert has not landed",
      section,
      value: status.culprits.length,
      limits: { amber: 1 },
      ...(status.culprits.length
        ? {
            why: status.culprits
              .map(
                (culprit) =>
                  `${short(culprit.commit)} (${culprit.part}: ${culprit.steps.join(", ")}), revert ${culprit.revert ? short(culprit.revert) : "none could be made"}`,
              )
              .join("; "),
          }
        : {}),
    },
    ...records,
    {
      id: "count",
      label: "the count",
      section,
      value: status.count && status.count.verdict !== "PASS" ? status.count.lines.length : 0,
      text: status.count
        ? status.count.verdict === "PASS"
          ? "PASS"
          : `${status.count.verdict}: ${status.count.lines.length} lines`
        : "not run yet",
      state: status.count && status.count.verdict !== "PASS" ? "amber" : "ok",
      ...(status.count && status.count.verdict !== "PASS"
        ? { why: "its red lines wait for your yes (Needs you)" }
        : {}),
    },
  ];
}

export function readFullCheck(value: unknown): FullCheckStatus {
  const status = checked<FullCheckStatus & { version: number }>(
    value,
    "full check",
    [1, 2],
    "full-check",
  );
  if (status.version === 1 && !status.core?.records)
    throw new Error("NTS full-check version 1 lacks per-part records");
  return status;
}
