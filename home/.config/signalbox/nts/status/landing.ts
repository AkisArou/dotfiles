// Read-only NTS landing projections, ported from ~/.config/signalbox/nts/landing.ts.
import type { Gauge, Report, TableCell } from "./model.ts";

/** A change in the queue, as the status prints it. */
interface Change {
  readonly id: string;
  readonly sha: string;
  readonly branch: string | null;
  readonly issue: number | null;
  readonly by: string;
  readonly submitted_at: string;
  readonly note: string | null;
}

/** An answer: landed, refused, withdrawn or held. */
interface Answer {
  readonly id: string;
  readonly sha: string;
  readonly state: "landed" | "refused" | "withdrawn" | "held";
  readonly at: string;
  readonly message: string;
  readonly branch: string | null;
  readonly issue: number | null;
  readonly run: string | null;
  readonly wall_s: number | null;
  readonly estimate_s: number | null;
  readonly landing: readonly string[];
  readonly deferred: readonly string[];
  readonly pending: number;
}

export interface LandingStatus {
  readonly service: {
    readonly running: boolean;
    readonly state: string;
    readonly since: string;
    readonly message: string | null;
  };
  readonly checking:
    | (Change & {
        readonly phase: string | null;
        readonly run: string | null;
        readonly started_at: string;
        readonly seconds: number;
      })
    | null;
  readonly waiting: readonly Change[];
  readonly held: readonly (Change & { readonly message: string })[];
  readonly day: {
    readonly landed: number;
    readonly refused: number;
    readonly held: number;
    readonly withdrawn: number;
    readonly median_wall_s: number | null;
    readonly max_wall_s: number | null;
  };
  readonly recent: readonly Answer[];
  readonly main: { readonly commit: string | null; readonly pushed: boolean | null };
}

const named = (change: {
  readonly issue: number | null;
  readonly branch: string | null;
  readonly sha: string;
}) => `${change.issue ? `#${change.issue} ` : ""}${change.branch ?? change.sha.slice(0, 12)}`;
/** The landing check's goal for one change, in minutes. */
export const GOAL_MIN = 10;

/**
 * The landing check's gauges: the day's median and longest against the
 * ten-minute goal, the share refused, the changes held (a person must act),
 * the changes refused and then landed unchanged (each a flaky test to fix),
 * and whether the service runs.
 */
export function landingGauges(status: LandingStatus, now = Date.now()): Gauge[] {
  const { day } = status;
  // The service runs while it drains the queue: idle with nothing waiting is as it should be,
  // and a change left waiting a minute with no service draining it is stuck.
  const oldest = Math.min(...status.waiting.map((change) => Date.parse(change.submitted_at)));
  const stuck = !status.service.running && status.waiting.length > 0 && now - oldest > 60_000;
  const section = "landing";
  const answered = day.landed + day.refused;
  // The same commit refused, then landed: a test that failed and passed on the same code.
  const refusedAt = new Map<string, number>();
  const flaky = new Set<string>();
  for (const answer of [...status.recent].sort((a, b) => Date.parse(a.at) - Date.parse(b.at))) {
    if (answer.state === "refused") refusedAt.set(answer.sha, Date.parse(answer.at));
    else if (answer.state === "landed" && refusedAt.has(answer.sha)) flaky.add(answer.sha);
  }
  const minutesOf = (seconds: number | null) =>
    seconds === null ? undefined : Math.round((seconds / 60) * 10) / 10;
  const median = minutesOf(day.median_wall_s);
  const longest = minutesOf(day.max_wall_s);
  return [
    {
      id: "service",
      label: "the landing service",
      section,
      value: stuck ? 0 : 1,
      text: status.service.running
        ? `draining: ${status.service.state}`
        : stuck
          ? `not running, ${status.waiting.length} waiting`
          : status.service.state,
      state: stuck ? "red" : "ok",
      ...(stuck
        ? {
            why: "changes wait and no service drains them: systemctl --user status nts-landing-check says why",
          }
        : {}),
    },
    ...(median !== undefined
      ? [
          {
            id: "median",
            label: "median landing today",
            section,
            value: median,
            unit: "minutes",
            limits: { amber: 0.8 * GOAL_MIN, red: GOAL_MIN },
            why: `the goal is ${GOAL_MIN} minutes a change`,
          },
        ]
      : []),
    ...(longest !== undefined
      ? [
          {
            id: "longest",
            label: "longest landing today",
            section,
            value: longest,
            unit: "minutes",
            limits: { red: 1.5 * GOAL_MIN },
          },
        ]
      : []),
    {
      id: "refused",
      label: "refused today",
      section,
      value: answered ? Math.round((100 * day.refused) / answered) : 0,
      unit: "%",
      limits: { amber: 20 },
      why: `${day.refused} refused of ${answered} answered`,
    },
    {
      id: "held",
      label: "changes held",
      section,
      value: status.held.length,
      limits: { red: 1 },
      ...(status.held.length
        ? {
            why: `${status.held.map((change) => `${named(change)}: ${change.message}`).join("; ")}. use NTS tooling to retry or withdraw`,
          }
        : {}),
    },
    {
      id: "flaky",
      label: "the same commit refused, then landed",
      section,
      value: flaky.size,
      limits: { amber: 1, red: 2 },
      ...(flaky.size
        ? { why: `each is a test to fix: ${[...flaky].map((sha) => sha.slice(0, 12)).join(", ")}` }
        : {}),
    },
  ];
}

/** When a check ran, start to end in local time; the date only when it was not today. */
/** The landing service's answers, the most recent first. */
export function newestFirst<T extends Pick<Answer, "at">>(answers: readonly T[]): T[] {
  return [...answers].sort((a, b) => Date.parse(b.at) - Date.parse(a.at));
}

export function ranAt(answer: Pick<Answer, "at" | "wall_s">, now = Date.now()): string {
  const end = Date.parse(answer.at);
  if (!Number.isFinite(end)) return "";
  const start = answer.wall_s === null ? end : end - answer.wall_s * 1000;
  const clock = (time: number) => new Date(time).toTimeString().slice(0, 5);
  const finished = new Date(end);
  const day =
    finished.toDateString() === new Date(now).toDateString()
      ? ""
      : `${String(finished.getMonth() + 1).padStart(2, "0")}-${String(finished.getDate()).padStart(2, "0")} `;
  return answer.wall_s === null ? `${day}${clock(end)}` : `${day}${clock(start)}–${clock(end)}`;
}

export function landingReport(status: LandingStatus, now = Date.now()): Report {
  const queue = [
    ...(status.checking
      ? [{ ...status.checking, state: "checking", detail: status.service.message ?? "" }]
      : []),
    ...status.waiting.map((change) => ({ ...change, state: "waiting", detail: "" })),
    ...status.held.map((change) => ({ ...change, state: "held", detail: change.message })),
  ];
  const colored = (state: string): TableCell => ({
    text: state,
    color:
      state === "landed"
        ? "green"
        : state === "refused"
          ? "red"
          : state === "held"
            ? "yellow"
            : "gray",
  });
  return {
    claims: queue.flatMap((change) =>
      change.issue
        ? [
            {
              issue: change.issue,
              what:
                change.state === "checking"
                  ? "checking in the landing service"
                  : change.state === "held"
                    ? `held in the landing service: ${change.detail}`
                    : "queued to land",
            },
          ]
        : [],
    ),
    tables: [
      {
        id: "landing-state",
        title: "NTS landing service",
        columns: [
          { key: "what", label: "" },
          { key: "state", label: "" },
        ],
        rows: [
          {
            what: "service",
            state: { text: status.service.state, color: status.service.running ? "green" : "gray" },
          },
          { what: "main", state: status.main.commit?.slice(0, 12) ?? "unknown" },
          {
            what: "today",
            state: `${status.day.landed} landed, ${status.day.refused} refused, ${status.day.withdrawn} withdrawn, ${status.day.held} held`,
          },
          {
            what: "median",
            state:
              status.day.median_wall_s === null
                ? "unknown"
                : `${(status.day.median_wall_s / 60).toFixed(1)} min`,
          },
        ],
      },
      {
        id: "landing-queue",
        title: "Checking, waiting and held",
        key: "id",
        columns: [
          { key: "change", label: "change" },
          { key: "state", label: "state" },
          { key: "detail", label: "why" },
        ],
        rows: queue.map((change) => ({
          id: change.id,
          change: named(change),
          state: colored(change.state),
          detail: change.detail,
        })),
      },
      {
        id: "landing-recent",
        title: "Recent answers",
        key: "id",
        columns: [
          { key: "change", label: "change" },
          { key: "state", label: "state" },
          { key: "ran", label: "ran" },
          { key: "commit", label: "commit" },
          { key: "duration", label: "took", align: "right" },
          { key: "message", label: "answer" },
        ],
        rows: newestFirst(status.recent).map((answer) => ({
          id: `${answer.id}/${answer.at}/${answer.state}`,
          change: named(answer),
          state: colored(answer.state),
          ran: ranAt(answer, now),
          commit: answer.sha.slice(0, 12),
          duration: answer.wall_s === null ? "" : `${(answer.wall_s / 60).toFixed(1)} min`,
          message: answer.message,
          log: answer.run ? `${answer.run}/gate.log` : "",
        })),
      },
    ],
    gauges: landingGauges(status, now),
  };
}
