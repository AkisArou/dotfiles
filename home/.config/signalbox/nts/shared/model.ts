// NTS application data shared by the host, web client and terminal client.
import { operation, source } from "signalbox/core/remote";
import { issueStates, taskConditions, taskPhases } from "./glossary.ts";
import type { Term } from "./glossary.ts";

export type Issue = {
  id: string;
  number: number;
  title: string;
  body: string;
  url: string;
  area: string;
  stream: string;
  group: string;
  labels: string[];
  state: string;
  assignees: string[];
  owner: string | null;
  blockedBy: number[];
  workspace?: string;
  claims?: readonly import("../status/model.ts").WorkClaim[];
  parent: { number: number; title: string } | null;
  progress: { total: number; completed: number } | null;
};
export type Agent = {
  id: string;
  name: string;
  role: string;
  state: string;
  path: string | null;
  issue: number | null;
  terminal?: string;
};
export type Overview = { name: string; root: string; repo: string; startedAt: number };
export const overview = source<Overview>("nts.overview");
export const issues = source<Issue[]>("nts.issues");
export const agents = source<Agent[]>("nts.agents");
export const plan = source<string>("nts.plan");
/** NTS's label meanings (name to description), from its .github/labels.json. */
export const labels = source<Record<string, string>>("nts.labels");
export const terminalPrograms = source<{ id: string; label: string }[]>("nts.terminal-programs");
export const startTerminal = operation<{ program: string }, { id: string }>("nts.start-terminal");
/** The tmux pane a terminal's program runs in, for native tools such as a scrollback viewer. */
export type TerminalPane = { socketPath: string; pane: string };
export const terminalPane = operation<{ terminal: string }, TerminalPane | null>(
  "nts.terminal-pane",
);
// Orchestration: browser-safe views of host state, and the owner operations on it.
export type Harness = "claude" | "codex";
export type AgentRole = "coordinator" | "worker" | "reviewer";
/** One harness, model and effort; id is its stable name (claude/claude-opus-5-5/high). */
export type ProfileView = {
  id: string;
  label: string;
  harness: Harness;
  model?: string;
  effort: string;
};
/** How a launch chooses among a role's models. */
export type Pick = "first" | "spread" | "coordinator";
export const pickLabels: Record<Pick, string> = {
  first: "the first available, in order",
  spread: "spread across them (fewest live agents first)",
  coordinator: "the coordinator names one per task (else the first available)",
};
/** The agent settings in force, from settings.json, with the host's pause and availability. */
export type SettingsView = {
  /** settings.json, the only source of worker slots, rounds and the roles' models. */
  path: string;
  /** The file's version; a change names the version it read. */
  version: string;
  /** Why the file on disk is not in force (the last good settings are), or null. */
  error: string | null;
  workerSlots: number;
  maxRounds: number;
  /** How workerSlots is shared between groups of areas, or null when nothing is split. */
  shares: { name: string; percent: number; areas: string[]; running: number }[] | null;
  roles: Record<AgentRole, { pick: Pick; models: ProfileView[] }>;
  paused: { reason: string } | null;
  unavailable: Partial<Record<Harness, { reason: string; until?: number }>>;
  /** Every profile the harnesses offer. */
  catalog: ProfileView[];
};
export type AssignmentView = {
  id: string;
  key: string;
  status: string;
  issue: number | null;
  phase: string | null;
  wait: string | null;
  /** What it waits for, when it waits to be admitted: "resource:worker", "route:nts/task". */
  waitingOn: string[];
  /** The task that gets it first, when one queues ahead (its key, issue/<n>). */
  ahead: string | null;
  /** Its place in the queue is not known yet (the host is still reading the issues). */
  unordered: boolean;
  help: string | null;
  rounds: number;
  commit: string | null;
  tree: string | null;
  /** When the current phase began (ms since epoch). */
  phaseSince: number | null;
  /** The awaited agent's latest progress note. */
  progress: string | null;
  /** When the agent expected that progress to be done (ms since epoch). */
  progressUntil: number | null;
  /** The awaited agent stayed idle through its nudges. */
  stalled: string | null;
  /** What the coordinator must decide while the task is escalated. */
  escalation: string | null;
  /** The latest instruction the worker received (findings, a refusal, a decision). */
  note: string;
  design: string;
  summary: string;
  worker: string | null;
  reviewer: string | null;
  error: string | null;
  acceptedAt: number;
};
/** Where an issue goes once its question is answered. */
export type IssueState = "ready" | "parked" | "held";
/** One question for the owner: its choices, and once answered, the answer and what it did. */
export type QuestionView = {
  id: string;
  revision: number;
  status: "pending" | "decided" | "expired" | "withdrawn";
  question: string;
  context: string;
  issue: { number: number; title: string; url: string } | null;
  options: { label: string; then?: IssueState }[];
  askedBy: "coordinator" | "owner";
  requestedAt: number;
  answer: string | null;
  then: IssueState | null;
  answeredAt: number | null;
  /** The answer on the issue: "done", "waiting", or why the last try failed (retried). */
  posted: string | null;
};
/** What waits on the owner: open questions, issues nobody has asked about yet, recent answers. */
export type NeedsView = {
  questions: QuestionView[];
  unasked: { number: number; title: string; url: string }[];
  answered: QuestionView[];
};
/** How many things wait on the owner: open questions and issues nobody has asked about. */
export function needsCount(needs: NeedsView): number {
  return needs.questions.length + needs.unasked.length;
}
export const issueStateLabels: Record<IssueState, string> = {
  ready: "ready to start",
  parked: "parked (waits on your word)",
  held: "held (waits on what the issue names)",
};
export const settings = source<SettingsView>("nts.settings");
/** Changes settings.json through the host; role models are profile ids. */
export const setSettings = operation<
  {
    workerSlots?: number;
    maxRounds?: number;
    roles?: Partial<Record<AgentRole, { pick?: Pick; models?: string[] }>>;
    /** One group's share as a percent; the others are scaled to the rest. */
    share?: { name: string; percent: number };
    version?: string;
  },
  { version: string }
>("nts.settings.set");
/** Queued tasks wait and none is admitted; admitted tasks carry on. */
export const pauseTasks = operation<{ reason: string }, { ok: true }>("nts.tasks.pause");
export const resumeTasks = operation<Record<string, never>, { ok: true }>("nts.tasks.resume");
/** One line for the TUI's bottom bar and sbn status; a pause and a file error come first, so truncation keeps them. */
export function settingsLine(s: SettingsView): string {
  return `${s.paused ? `PAUSED: ${s.paused.reason} · ` : ""}${s.error ? "settings.json has an error · " : ""}workers ${s.workerSlots}${s.shares ? ` (${sharesLine(s)})` : ""} · rounds ${s.maxRounds}`;
}
/** The worker shares in one line: compiler 50% · tooling 50%. */
export function sharesLine(s: SettingsView): string {
  return (s.shares ?? []).map((group) => `${group.name} ${group.percent}%`).join(" · ");
}
/** A role's models in order, joined for one line. */
export function modelsLine(role: { models: ProfileView[] }): string {
  return role.models.map((model) => model.label).join(" → ");
}
export const assignments = source<AssignmentView[]>("nts.assignments");
export const needs = source<NeedsView>("nts.needs");
export const setAvailability = operation<
  { harness: Harness; unavailable: { reason: string; until?: number } | null },
  { ok: true }
>("nts.harness.availability");
export const assignTask = operation<
  { issue: number; instructions: string; model?: string },
  { id: string }
>("nts.tasks.assign");
export const cancelAssignment = operation<{ assignment: string; reason: string }, { ok: true }>(
  "nts.assignments.cancel",
);
/** Answers a question: one of its options, or written text (and, about an issue, where it goes). */
export const answerQuestion = operation<
  | { id: string; revision: number; option: number }
  | { id: string; revision: number; answer: string; then?: IssueState },
  { ok: true }
>("nts.needs.answer");
/** Withdraws an open question: moot, or to be asked again better. The coordinator is told. */
export const withdrawQuestion = operation<{ id: string; revision: number }, { ok: true }>(
  "nts.needs.withdraw",
);
/** Answers an issue that waits on the owner but has no question yet. */
export const answerIssue = operation<
  { issue: number; answer: string; then: IssueState },
  { ok: true }
>("nts.needs.answer-issue");
export const reworkAssignment = operation<{ assignment: string; notes: string }, { ok: true }>(
  "nts.assignments.rework",
);
/** What an assignment says now: its fault, stall, open question or wait, and its latest progress. */
/**
 * How a state reads, the same in the TUI and the web: each maps a tone to its
 * own colours. good: working or done well; active: under way; attention: wants
 * a look or a decision; bad: failed; quiet: gone; plain: nothing to say.
 */
export type Tone = "good" | "active" | "attention" | "bad" | "quiet" | "plain";

/**
 * An agent's lines under its name, the same in the TUI and the web: the issue it works on (by its
 * title), then its role and tree. path shortens the tree for display (the TUI writes ~ for home).
 */
export function agentLines(
  agent: Agent,
  issues: readonly Issue[],
  path: (tree: string) => string = (tree) => tree,
): string[] {
  const issue = agent.issue ? issues.find((item) => item.number === agent.issue) : undefined;
  return [
    ...(agent.issue ? [`#${agent.issue}${issue ? ` ${issue.title}` : ""}`] : []),
    `${agent.role} · ${agent.path ? path(agent.path) : "native terminal"}`,
  ];
}

/** A session: busy is working, idle waits for a message, ended is gone; an unknown state wants a look. */
export function agentTone(state: string): Tone {
  if (state === "busy" || state === "running") return "good";
  if (state === "idle" || state === "external") return "plain";
  if (state === "ended" || state === "exited") return "quiet";
  return "attention";
}

/** An issue: held, parked, waiting on the owner, unscoped or orphaned (work nobody is doing) wants a look. */
export function issueTone(state: string): Tone {
  return ["held", "parked", "needs-user", "unscoped", "orphaned"].includes(state)
    ? "attention"
    : "plain";
}

/** A task: a fault is bad; an escalation, a stall or a question wants a look; finished is quiet. */
export function assignmentTone(a: AssignmentView): Tone {
  if (a.error) return "bad";
  if (a.escalation || a.stalled || a.help) return "attention";
  if (a.status === "finished" || a.status === "cancelled") return "quiet";
  return "plain";
}

/** An answer's way to its issue: posted is done, posting is under way, a failure is bad (retried). */
export function postedTone(posted: string | null): Tone {
  return posted === "done" ? "quiet" : posted === "waiting" ? "active" : "bad";
}

/** A health reading: red is bad, amber wants a look, ok is good. */
export function gaugeTone(state: "red" | "amber" | "ok"): Tone {
  return state === "red" ? "bad" : state === "amber" ? "attention" : "good";
}

export function assignmentSays(a: AssignmentView, now = Date.now()): string {
  const said =
    a.error ??
    (a.escalation && `decide: ${a.escalation}`) ??
    (a.stalled && `stalled: ${a.stalled}`) ??
    (a.help && `asks: ${a.help}`) ??
    a.wait ??
    "";
  if (!a.progress || a.error) return said;
  const late =
    a.progressUntil !== null && now > a.progressUntil
      ? ` (expected done ${Math.round((now - a.progressUntil) / 60_000)} min ago)`
      : "";
  return `${said} · ${a.progress}${late}`;
}
/**
 * A task's progress in a word or two, for lists: what it needs from someone, its place in
 * the queue for a worker, or its phase.
 */
export function taskWord(a: AssignmentView): string {
  if (a.error) return "faulted";
  if (a.escalation) return "escalated";
  if (a.stalled) return "stalled";
  if (a.help) return "asks";
  if (a.unordered) return taskConditions.queued!.label;
  if (a.waitingOn.includes("resource:worker")) {
    const ahead = a.ahead && /^issue\/(\d+)/.exec(a.ahead)?.[1];
    return a.ahead ? `queued after ${ahead ? `#${ahead}` : a.ahead}` : taskConditions.next!.label;
  }
  if (a.status === "queued") return taskConditions.queued!.label;
  return phaseLabel(a.phase ?? a.status);
}

/** A phase as the lists say it ("building", "in review"); an unknown one as it is. */
export const phaseLabel = (phase: string) => taskPhases[phase]?.label ?? phase;

/** What a task's word means (the glossary's), for a tooltip or the help. */
export function taskMeaning(a: AssignmentView): string {
  if (a.error) return taskConditions.faulted!.meaning;
  if (a.escalation) return taskConditions.escalated!.meaning;
  if (a.stalled) return taskConditions.stalled!.meaning;
  if (a.help) return taskConditions.asks!.meaning;
  if (a.unordered) return taskConditions.queued!.meaning;
  if (a.waitingOn.includes("resource:worker"))
    return (a.ahead ? taskConditions.after! : taskConditions.next!).meaning;
  if (a.status === "queued") return taskConditions.queued!.meaning;
  return taskPhases[a.phase ?? ""]?.meaning ?? "";
}

/** An issue's state as the lists say it ("in progress"), and what it means. */
export const issueStateTerm = (state: string): Term =>
  issueStates[state] ?? { label: state, meaning: "" };

/** An issue's task in flight (queued or active), the latest when there are several. */
export function liveTask(
  assignments: readonly AssignmentView[],
  issue: number,
): AssignmentView | undefined {
  return assignments
    .filter((a) => a.issue === issue && (a.status === "queued" || a.status === "active"))
    .sort((a, b) => b.acceptedAt - a.acceptedAt)[0];
}

/** The task word and how long its phase has run, e.g. "building · 42 min". */
export function assignmentPhase(a: AssignmentView, now = Date.now()): string {
  // Its word (a queue place, a phase, what it needs), then how long the phase has run.
  const word = taskWord(a);
  return a.phaseSince === null
    ? word
    : `${word} · ${Math.max(0, Math.round((now - a.phaseSince) / 60_000))} min`;
}
export const prioritizeTask = operation<
  { issue: number; urgency: number },
  { urgency: Record<string, number> }
>("nts.tasks.prioritize");
export const signOff = operation<
  { assignment: string; accept: boolean; notes: string },
  { ok: true }
>("nts.assignments.sign-off");
/** A fresh coordinator generation from its checkpoint: after its tools or instructions change. */
export const replaceCoordinator = operation<{ reason: string }, { ok: true }>(
  "nts.coordinator.replace",
);
/** Ends one managed agent session (a stray or replaced one); tasks end their own. */
export const endAgent = operation<{ id: string }, { ok: true }>("nts.agents.end");
export const retryAssignment = operation<{ assignment: string }, { ok: true }>(
  "nts.assignments.retry",
);
export const decideDesign = operation<
  { assignment: string; accept: boolean; notes: string },
  { ok: true }
>("nts.assignments.decide-design");
export const messageWorker = operation<
  { assignment: string; text: string; from?: "user" | "coordinator" },
  { ok: true }
>("nts.assignments.message-worker");
export const tabs = [
  "Now",
  // Second: what waits on the owner is one key away from anywhere.
  "Needs you",
  "Tasks",
  "Agents",
  "Landing",
  "Full check",
  "Benchmarks",
  "Conformance",
  "Plan",
  "Machine",
  "Health",
] as const;
export type Tab = (typeof tabs)[number];
export const tabKey = (index: number) => (index < 9 ? String(index + 1) : index === 9 ? "0" : "");
