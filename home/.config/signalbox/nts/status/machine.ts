// NTS machine projections, ported from ~/.config/signalbox/nts/machine.ts.
import type { Gauge, Report, Table, TableCell, TableRow } from "./model.ts";

const HEAVY_GIB = 14;
const STOP_GIB = 12;
const GIB = 1024 ** 3;
const gib = (bytes: number) => `${(bytes / GIB).toFixed(1)} GiB`;
const gb = (bytes: number | null) => (bytes === null ? "" : `${Math.round(bytes / 1e9)} GB`);
const ago = (seconds: number) =>
  seconds < 3600
    ? `${Math.round(seconds / 60)}m`
    : seconds < 86_400
      ? `${Math.round(seconds / 3600)}h`
      : `${Math.round(seconds / 86_400)}d`;

/**
 * What `pool.ts status --json` prints (`version: 1`, or 2 from #884, which adds a held or
 * landed tree's worker presence; this application does not read it yet).
 */
export interface PoolStatus {
  readonly pool: string;
  readonly room: {
    readonly ok: boolean;
    readonly free_bytes: number;
    readonly floor_bytes: number;
    readonly unbuilt: number;
    readonly why: string | null;
  };
  readonly trees: readonly {
    readonly name: string;
    readonly path: string;
    readonly state: string;
    readonly task: string | null;
    readonly ahead: number;
    readonly behind: number;
    readonly uncommitted: number;
    readonly idle_ms: number | null;
    readonly target_bytes: number | null;
    /** Version 2, held and landed trees: whether a worker is present in the tree. */
    readonly worker_presence?: "attached" | "detached" | "unknown";
    readonly detached_at?: number | null;
    readonly presence_diagnostic?: string | null;
  }[];
}

/** What `admit.ts --status --json` prints (version 1, kind "memory"): the ledger's reservations and its queue. */
export interface LedgerStatus {
  readonly ledger: string;
  readonly machine: {
    readonly total_bytes: number;
    readonly available_bytes: number;
    readonly floor_bytes: number;
  };
  /** What admitted jobs may still take, and what the queue has to work with. */
  readonly pending_bytes: number;
  readonly room_bytes: number;
  /**
   * `unmeasured`: the steps or jobs it sized with no measurement of their own;
   * `over`: those that held more than they reserved. Both are absent from an
   * nts older than #590.
   */
  readonly admitted: readonly {
    readonly id: string;
    readonly what: string;
    readonly priority: string;
    readonly bytes: number;
    readonly used_bytes: number;
    readonly owner_pid: number;
    readonly since: string;
    readonly live: boolean;
    readonly unmeasured?: readonly string[];
    readonly over?: readonly string[];
  }[];
  /** In queue order. A request to grow an admitted reservation names it in `grows`; its `bytes` are what it grows by. */
  readonly waiting: readonly {
    readonly id: string;
    readonly what: string;
    readonly priority: string;
    readonly bytes: number;
    readonly grows: string | null;
    readonly owner_pid: number;
    readonly waiter_pid: number;
    readonly since: string;
    readonly waited_s: number;
    readonly live: boolean;
  }[];
}

/** The ledger's limits (MainClaude, 11-09): a landing kept waiting past two minutes is red, anything past fifteen amber. */
const LANDING_WAIT_S = 120;
const ANY_WAIT_S = 900;

/** How a reservation was sized: the keys it held past their size (its history takes the peak for next time), and those it had no measurement for. */
function sizing(entry: LedgerStatus["admitted"][number]): TableCell {
  const over = entry.over ?? [];
  const unmeasured = entry.unmeasured ?? [];
  const said = [
    ...(over.length ? [`held more than reserved: ${over.join(" ")}`] : []),
    ...(unmeasured.length ? [`unmeasured: ${unmeasured.join(" ")}`] : []),
  ].join("; ");
  return over.length
    ? { text: said, color: "red" }
    : unmeasured.length
      ? { text: said, color: "yellow" }
      : "";
}

/** The memory ledger as a table: what is admitted, then what waits, in its order. */
export function ledgerTable(status: LedgerStatus, nowMs: number): Table {
  return {
    id: "memory-ledger",
    title: `Memory: ${gib(status.room_bytes)} for the queue, ${gib(status.pending_bytes)} still to be taken by what is admitted`,
    key: "id",
    columns: [
      { key: "state", label: "" },
      { key: "priority", label: "priority" },
      { key: "bytes", label: "reserved", align: "right" },
      { key: "used", label: "uses", align: "right" },
      { key: "since", label: "for", align: "right" },
      { key: "what", label: "what" },
      { key: "sizing", label: "sizing" },
    ],
    rows: [
      ...status.admitted.map((entry) => ({
        id: entry.id,
        state: entry.live
          ? { text: "admitted", color: "green" as const }
          : { text: "its owner has exited", color: "gray" as const },
        priority: entry.priority,
        bytes: gib(entry.bytes),
        used: gib(entry.used_bytes),
        since: ago((nowMs - Date.parse(entry.since)) / 1000),
        what: entry.what,
        sizing: sizing(entry),
      })),
      ...status.waiting.map((entry, place) => {
        const late =
          (entry.priority === "landing" && entry.waited_s > LANDING_WAIT_S) ||
          entry.waited_s > ANY_WAIT_S;
        return {
          id: entry.id,
          state: entry.live
            ? {
                text: `waiting, ${place + 1} of ${status.waiting.length}`,
                color: late ? ("red" as const) : ("yellow" as const),
              }
            : { text: "gone", color: "gray" as const },
          priority: entry.priority,
          bytes: entry.grows === null ? gib(entry.bytes) : `grows by ${gib(entry.bytes)}`,
          used: "",
          since: ago(entry.waited_s),
          what: entry.what,
          sizing: "",
        };
      }),
    ],
  };
}

export function machineGauges(
  ledger: LedgerStatus,
  pool: PoolStatus,
  oomKills: number | undefined,
): Gauge[] {
  const waiting = ledger.waiting.filter((entry) => entry.live);
  const longest = (entries: typeof waiting) =>
    entries.reduce((most, entry) => Math.max(most, entry.waited_s), 0);
  const landing = waiting.filter((entry) => entry.priority === "landing");
  const minutes = (seconds: number) => Math.round((seconds / 60) * 10) / 10;
  const freeGb = Math.round(pool.room.free_bytes / 1e9);
  const floorGb = Math.round(pool.room.floor_bytes / 1e9);
  return [
    {
      id: "memory",
      label: "memory available",
      section: "machine",
      value: Math.round((ledger.machine.available_bytes / GIB) * 10) / 10,
      unit: "GiB",
      limits: { amber: HEAVY_GIB, red: STOP_GIB, worse: "lower" },
      why: `below ${HEAVY_GIB} GiB no heavy phase starts, below ${STOP_GIB} no new case (AGENTS.md, The machine)`,
    },
    {
      id: "memory.landing-wait",
      label: "a landing waiting for memory",
      section: "machine",
      value: minutes(longest(landing)),
      unit: "minutes",
      ...(landing.length ? {} : { text: "none waits" }),
      limits: { red: LANDING_WAIT_S / 60 },
      ...(landing.length
        ? {
            why: landing
              .map((entry) => `${entry.what}: ${gib(entry.bytes)}, ${ago(entry.waited_s)}`)
              .join("; "),
          }
        : {}),
    },
    {
      id: "memory.wait",
      label: "longest wait for memory",
      section: "machine",
      value: minutes(longest(waiting)),
      unit: "minutes",
      ...(waiting.length ? {} : { text: "none waits" }),
      limits: { amber: ANY_WAIT_S / 60 },
      ...(waiting.length
        ? { why: `${waiting.length} waiting; first ${waiting[0]?.what ?? ""}` }
        : {}),
    },
    {
      id: "disk",
      label: "disk for the pool",
      section: "machine",
      value: freeGb,
      unit: "GB",
      // Near the floor is within a quarter of it; past it, no tree can be made.
      limits: { amber: Math.round(floorGb * 1.25), red: floorGb, worse: "lower" },
      ...(pool.room.ok
        ? {}
        : { state: "red" as const, why: pool.room.why ?? "no room for another tree" }),
    },
    ...(oomKills === undefined
      ? []
      : [
          {
            id: "oom",
            label: "kills for want of memory today",
            section: "machine",
            value: oomKills,
            limits: { red: 1 },
            ...(oomKills
              ? { why: "journalctl -k -g oom-kill:constraint=CONSTRAINT_NONE shows them" }
              : {}),
          },
        ]),
  ];
}

function poolTable(status: PoolStatus): Table {
  return {
    id: "pool",
    title: `The pool: ${status.pool}`,
    key: "tree",
    columns: [
      { key: "tree", label: "tree", align: "right" },
      { key: "state", label: "state" },
      { key: "task", label: "task" },
      { key: "ahead", label: "ahead", align: "right" },
      { key: "behind", label: "behind", align: "right" },
      { key: "uncommitted", label: "uncommitted", align: "right" },
      { key: "idle", label: "idle", align: "right" },
      { key: "target", label: "target", align: "right" },
    ],
    rows: status.trees.map((tree) => ({
      tree: tree.name,
      state: {
        text: tree.state,
        color: tree.state === "free" ? "green" : tree.state === "held" ? "cyan" : "yellow",
      },
      task: tree.task ?? "",
      ahead: tree.ahead,
      behind: tree.behind,
      uncommitted: tree.uncommitted ? { text: String(tree.uncommitted), color: "yellow" } : 0,
      idle: tree.idle_ms === null ? "" : ago(tree.idle_ms / 1000),
      target: gb(tree.target_bytes),
    })),
  };
}

const UNIT_COLUMNS = [
  { key: "unit", label: "unit" },
  { key: "state", label: "state" },
  { key: "since", label: "since", align: "right" as const },
  { key: "memory", label: "memory", align: "right" as const },
  { key: "cpu", label: "CPU time", align: "right" as const },
];

/** `systemctl show`'s blocks of properties as the units table's rows: a failed unit in red, an active one in green. */
export function unitRows(shown: string, nowMs: number): TableRow[] {
  const now = nowMs / 1000;
  return shown
    .split("\n\n")
    .map(
      (block) =>
        Object.fromEntries(
          block
            .split("\n")
            .map((line) => [line.slice(0, line.indexOf("=")), line.slice(line.indexOf("=") + 1)]),
        ) as Record<string, string>,
    )
    .filter((unit) => unit["Id"])
    .map((unit) => {
      const active = unit["ActiveState"] ?? "";
      const memory = Number(unit["MemoryCurrent"]);
      const cpu = Number(unit["CPUUsageNSec"]);
      const changed = Number(unit["StateChangeTimestamp"]?.replace(/^@/, ""));
      return {
        unit: (unit["Id"] ?? "").replace(/\.service$/, ""),
        state: {
          text: `${active} ${unit["SubState"] ?? ""}`.trim(),
          ...(active === "failed"
            ? { color: "red" as const }
            : active === "active"
              ? { color: "green" as const }
              : {}),
        },
        since: Number.isFinite(changed) && changed > 0 ? ago(now - changed) : "",
        memory: Number.isFinite(memory) ? gib(memory) : "",
        cpu: Number.isFinite(cpu) ? ago(cpu / 1e9) : "",
      };
    });
}

export interface MachineSnapshot {
  readonly pool: PoolStatus;
  readonly ledger: LedgerStatus;
  readonly units: string;
  readonly load: string;
  readonly measurementLock: TableCell;
  readonly oomKills?: number;
}
export function machineReport(snapshot: MachineSnapshot, now = Date.now()): Report {
  const { pool, ledger } = snapshot;
  const available = ledger.machine.available_bytes / GIB;
  const machine: Table = {
    id: "machine",
    title: "The machine",
    columns: [
      { key: "what", label: "" },
      { key: "state", label: "" },
    ],
    rows: [
      {
        what: "memory",
        state: {
          text: `${gib(ledger.machine.available_bytes)} available of ${gib(ledger.machine.total_bytes)}`,
          color: available < STOP_GIB ? "red" : available < HEAVY_GIB ? "yellow" : "green",
        },
      },
      { what: "CPUs 4-5", state: snapshot.measurementLock },
      {
        what: "disk",
        state: {
          text: pool.room.ok
            ? `room for another tree: ${gb(pool.room.free_bytes)} free, ${gb(pool.room.floor_bytes)} needed`
            : (pool.room.why ?? "no room for another tree"),
          color: pool.room.ok ? "green" : "red",
        },
      },
      { what: "load", state: snapshot.load },
    ],
  };
  return {
    claims: pool.trees.flatMap((tree) => {
      const issue = tree.state === "held" ? /^(\d+)(?:-|$)/.exec(tree.task ?? "")?.[1] : undefined;
      if (!issue) return [];
      return [
        {
          issue: Number(issue),
          what: `tree ${tree.name}${tree.ahead ? ` · ${tree.ahead} unlanded` : ""}`,
          path: tree.path,
          ...(tree.idle_ms === null ? {} : { idleMs: tree.idle_ms }),
        },
      ];
    }),
    tables: [
      machine,
      ledgerTable(ledger, now),
      {
        id: "units",
        title: "NTS services",
        key: "unit",
        columns: UNIT_COLUMNS,
        rows: unitRows(snapshot.units, now),
      },
      poolTable(pool),
    ],
    gauges: machineGauges(ledger, pool, snapshot.oomKills),
  };
}
