// nts's landing check (docs/workflow.md, Landing), for signalbox, read from
// `node tooling/landing/check.ts status --json` (version 1): nts's tool
// stays as it is. Each change is a run on the Landing tab: checking, waiting,
// held, and the recent answers; the last day is the tab's summary; a held
// change waits on you. An answer is a `landing.result` event with its
// minutes, so Health can measure landing time; the day's numbers are gauges,
// held to the ten-minute goal (docs/workflow.md, Landing).

import { command, type Action, type Claim, type Decision, type Gauge, type Provider, type Report, type Run, type RunState } from "signalbox";
import { checked, NTS_ISSUES } from "./status.ts";

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
  readonly service: { readonly running: boolean; readonly state: string; readonly since: string; readonly message: string | null };
  readonly checking: (Change & { readonly phase: string | null; readonly run: string | null; readonly started_at: string; readonly seconds: number }) | null;
  readonly waiting: readonly Change[];
  readonly held: readonly (Change & { readonly message: string })[];
  readonly day: { readonly landed: number; readonly refused: number; readonly held: number; readonly withdrawn: number; readonly median_wall_s: number | null; readonly max_wall_s: number | null };
  readonly recent: readonly Answer[];
  readonly main: { readonly commit: string | null; readonly pushed: boolean | null };
}

/** The provider's id: the Landing tab shows its runs alone. */
export const LANDING = "nts-landing";
const ISSUES = "https://github.com/AkisArou/nts/issues";

type Fact = readonly [string, string];
const fact = (label: string, value: string): Fact => [label, value];
const minutes = (seconds: number | null | undefined) => (seconds === null || seconds === undefined ? "?" : `${(seconds / 60).toFixed(1)} min`);
const clock = (iso: string) => new Date(iso).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", hour12: false });
/** What a change is: its issue and branch, or its commit. */
const named = (change: { readonly issue: number | null; readonly branch: string | null; readonly sha: string }) =>
  `${change.issue ? `#${change.issue} ` : ""}${change.branch ?? change.sha.slice(0, 12)}`;
const issueUrl = (issue: number | null) => (issue ? { url: `${ISSUES}/${issue}` } : {});
const STATE: Record<Answer["state"], RunState> = { landed: "landed", refused: "refused", withdrawn: "cancelled", held: "held" };

function answerRun(answer: Answer): Run {
  const finishedAt = Date.parse(answer.at);
  return {
    id: answer.id,
    title: named(answer),
    state: STATE[answer.state],
    commit: answer.sha,
    ...(answer.branch ? { branch: answer.branch } : {}),
    ...(answer.wall_s !== null ? { startedAt: finishedAt - answer.wall_s * 1000 } : {}),
    finishedAt,
    ...issueUrl(answer.issue),
    kind: "landing.result",
    facts: [
      fact("answer", answer.message),
      fact("took", `${minutes(answer.wall_s)} of an estimated ${minutes(answer.estimate_s)}`),
      fact("steps", `${answer.landing.length} run, ${answer.deferred.length} deferred to the full check`),
      ...(answer.pending ? [fact("records", `${answer.pending} pending`)] : []),
      ...(answer.run ? [fact("log", `${answer.run}/gate.log`)] : []),
    ],
    data: {
      ...(answer.wall_s !== null ? { minutes: answer.wall_s / 60 } : {}),
      ...(answer.estimate_s !== null ? { estimate: answer.estimate_s / 60 } : {}),
      steps: answer.landing.length,
      deferred: answer.deferred.length,
      pending: answer.pending,
      ...(answer.issue ? { issue: answer.issue } : {}),
    },
  };
}

/** The landing status as a report: runs, the day as a summary, held changes as decisions. */
export function landingReport(status: LandingStatus): Report {
  const { checking, day } = status;
  // The estimate is in the service's message: "build clippy … on cde715ec33b8, ~393 s".
  const estimate = Number(/~(\d+) s/.exec(status.service.message ?? "")?.[1] ?? NaN);
  const runs: Run[] = [
    ...(checking
      ? [
          {
            id: checking.id,
            title: named(checking),
            state: "running" as const,
            commit: checking.sha,
            ...(checking.branch ? { branch: checking.branch } : {}),
            startedAt: Date.parse(checking.started_at),
            ...issueUrl(checking.issue),
            kind: "landing.result",
            facts: [
              fact("phase", checking.phase ?? "?"),
              fact("elapsed", `${minutes(checking.seconds)}${Number.isNaN(estimate) ? "" : ` of an estimated ${minutes(estimate)}`}`),
              ...(status.service.message ? [fact("steps", status.service.message)] : []),
              ...(checking.run ? [fact("log", `${checking.run}/gate.log`)] : []),
            ],
          },
        ]
      : []),
    ...status.waiting.map((change, place) => ({
      id: change.id,
      title: named(change),
      state: "queued" as const,
      commit: change.sha,
      ...issueUrl(change.issue),
      kind: "landing.result",
      facts: [fact("place", String(place + 1)), fact("by", change.by), fact("since", clock(change.submitted_at)), ...(change.note ? [fact("note", change.note)] : [])],
    })),
    ...status.held.map((change) => ({
      id: change.id,
      title: named(change),
      state: "held" as const,
      commit: change.sha,
      ...issueUrl(change.issue),
      kind: "landing.result",
      facts: [fact("held", change.message), fact("by", change.by), fact("what to do", "R retries it, W withdraws it")],
    })),
    // An answer to a change still listed above is that change's: it is shown once, as it is now.
    ...status.recent.filter((answer) => answer.id !== checking?.id && !status.held.some((change) => change.id === answer.id)).map(answerRun),
  ];
  const decisions: Decision[] = status.held.map((change) => ({
    id: heldDecision(change.id),
    title: `a landing is held: ${named(change)}: ${change.message} (R retries, W withdraws)`,
    ...issueUrl(change.issue),
    updatedAt: Date.parse(change.submitted_at),
  }));
  // A change being checked, waiting or held claims its issue: no agent takes it again.
  const claim = (change: Change, what: string): Claim[] => (change.issue ? [{ task: { id: String(change.issue), provider: NTS_ISSUES }, what }] : []);
  const claims = [
    ...(checking ? claim(checking, `checking in the landing check, ${minutes(checking.seconds)}`) : []),
    ...status.waiting.flatMap((change, place) => claim(change, `queued to land, ${ordinal(place + 1)}`)),
    ...status.held.flatMap((change) => claim(change, `held in the landing check: ${change.message}`)),
  ];
  return {
    runs,
    decisions,
    claims,
    gauges: landingGauges(status),
    summary: [
      // The service runs while it drains the queue; between changes it is idle, as it should be.
      fact("service", `${status.service.running ? "draining: " : ""}${status.service.state} since ${clock(status.service.since)}`),
      fact("today", `${day.landed} landed, ${day.refused} refused, ${day.withdrawn} withdrawn${day.held ? `, ${day.held} held` : ""}`),
      fact("median", minutes(day.median_wall_s)),
      fact("longest", minutes(day.max_wall_s)),
      fact("main", `${status.main.commit?.slice(0, 12) ?? "?"}${status.main.pushed ? " pushed" : status.main.pushed === false ? " not pushed" : ""}`),
    ],
  };
}

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
  const minutesOf = (seconds: number | null) => (seconds === null ? undefined : Math.round((seconds / 60) * 10) / 10);
  const median = minutesOf(day.median_wall_s);
  const longest = minutesOf(day.max_wall_s);
  return [
    {
      id: "service",
      label: "the landing service",
      section,
      value: stuck ? 0 : 1,
      text: status.service.running ? `draining: ${status.service.state}` : stuck ? `not running, ${status.waiting.length} waiting` : status.service.state,
      state: stuck ? "red" : "ok",
      ...(stuck ? { why: "changes wait and no service drains them: systemctl --user status nts-landing-check says why" } : {}),
    },
    ...(median !== undefined ? [{ id: "median", label: "median landing today", section, value: median, unit: "minutes", limits: { amber: 0.8 * GOAL_MIN, red: GOAL_MIN }, why: `the goal is ${GOAL_MIN} minutes a change` }] : []),
    ...(longest !== undefined ? [{ id: "longest", label: "longest landing today", section, value: longest, unit: "minutes", limits: { red: 1.5 * GOAL_MIN } }] : []),
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
      ...(status.held.length ? { why: `${status.held.map((change) => `${named(change)}: ${change.message}`).join("; ")}. R retries, W withdraws (Landing tab)` } : {}),
    },
    {
      id: "flaky",
      label: "the same commit refused, then landed",
      section,
      value: flaky.size,
      limits: { amber: 1, red: 2 },
      ...(flaky.size ? { why: `each is a test to fix: ${[...flaky].map((sha) => sha.slice(0, 12)).join(", ")}` } : {}),
    },
  ];
}

const heldDecision = (id: string) => `landing-held:${id}`;
const ordinal = (n: number) => `${n}${n % 10 === 1 && n % 100 !== 11 ? "st" : n % 10 === 2 && n % 100 !== 12 ? "nd" : n % 10 === 3 && n % 100 !== 13 ? "rd" : "th"}`;

/** The landing check, read every 10 seconds from its main checkout. */
export function ntsLanding(): Provider {
  return command({ id: LANDING, label: "Landing check", command: ["node", "tooling/landing/check.ts", "status", "--json"], intervalSeconds: 10, parse: (json) => landingReport(checked<LandingStatus>(json, "landing", [1], "landing")) });
}

/** The change a selection names, when it is a held landing: on the Landing tab, or on Needs you. */
function heldChange(selection: Parameters<Action["run"]>[0]): string | undefined {
  if (selection.kind === "run" && selection.run.source?.id === LANDING && selection.run.state === "held") return selection.run.id;
  if (selection.kind === "decision" && selection.decision.id.startsWith("landing-held:")) return selection.decision.id.slice("landing-held:".length);
  return undefined;
}

/** `check.ts retry` or `withdraw` on a held change, once you answer y, its answer shown in a popup. */
function onHeld(key: string, verb: "retry" | "withdraw", root: string): Action {
  return {
    key,
    label: verb,
    help: `${verb} the held landing (node tooling/landing/check.ts ${verb} <id>), once you answer y`,
    when: (selection) => heldChange(selection) !== undefined,
    run: (selection, tools) => {
      const id = heldChange(selection);
      if (!id) return;
      tools.ask(`${verb} the held landing ${id}? y/n`, () =>
        tools.terminal(["sh", "-c", `node tooling/landing/check.ts ${verb} "$1"; printf '\\n(enter closes this)'; read -r _`, "sh", id], { cwd: root, title: `${verb} ${id}`, height: "40%" }),
      );
    },
  };
}

/** R and W on a held landing. */
export function landingActions(root: string): Action[] {
  return [onHeld("R", "retry", root), onHeld("W", "withdraw", root)];
}

/** A run's log: the landing check's run folder holds gate.log. */
export function landingLog(root: string): Action {
  return {
    key: "w",
    label: "log",
    help: "the landing run's gate log, in a popup",
    when: (selection) => selection.kind === "run" && selection.run.source?.id === LANDING && selection.run.facts?.some(([label]) => label === "log") === true,
    run: (selection, tools) => {
      if (selection.kind !== "run") return;
      const log = selection.run.facts?.find(([label]) => label === "log")?.[1];
      if (log) tools.terminal(["less", "-R", "+G", log], { cwd: root, title: `log · ${selection.run.title}` });
    },
  };
}
