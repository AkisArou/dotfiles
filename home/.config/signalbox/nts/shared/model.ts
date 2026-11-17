// NTS application data shared by the host, web client and terminal client.
import { operation, source } from "signalbox/core/remote";

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
export const terminalPrograms = source<{ id: string; label: string }[]>("nts.terminal-programs");
export const startTerminal = operation<{ program: string }, { id: string }>("nts.start-terminal");
// Orchestration: browser-safe views of host state, and the owner operations on it.
export type Harness = "claude" | "codex";
export type AgentRole = "coordinator" | "worker" | "reviewer";
export type ProfileView = {
  id: string;
  label: string;
  harness: Harness;
  model?: string;
  effort?: string;
};
export type ProfileSettingsView = {
  revision: number;
  profiles: ProfileView[];
  roles: Record<AgentRole, { profile: string; fallback?: string }>;
  unavailable: Partial<Record<Harness, { reason: string; until?: number }>>;
};
export type AssignmentView = {
  id: string;
  key: string;
  status: string;
  issue: number | null;
  phase: string | null;
  wait: string | null;
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
export type UserDecisionView = {
  id: string;
  status: string;
  revision: number;
  question: string;
  context: string;
  answer: string | null;
  requestedAt: number;
};
export const profileSettings = source<ProfileSettingsView>("nts.profiles");
export const assignments = source<AssignmentView[]>("nts.assignments");
export const userDecisions = source<UserDecisionView[]>("nts.user-decisions");
export const setProfile = operation<
  { profile: ProfileView; revision: number },
  { revision: number }
>("nts.profiles.set");
export const setRole = operation<
  { role: AgentRole; profile: string; fallback?: string; revision: number },
  { revision: number }
>("nts.roles.set");
export const setAvailability = operation<
  { harness: Harness; unavailable: { reason: string; until?: number } | null },
  { revision: number }
>("nts.harness.availability");
export const assignTask = operation<{ issue: number; instructions: string }, { id: string }>(
  "nts.tasks.assign",
);
export const cancelAssignment = operation<{ assignment: string; reason: string }, { ok: true }>(
  "nts.assignments.cancel",
);
export const answerDecision = operation<
  { id: string; revision: number; answer: string },
  { ok: true }
>("nts.user-decisions.answer");
export const reworkAssignment = operation<{ assignment: string; notes: string }, { ok: true }>(
  "nts.assignments.rework",
);
/** What an assignment says now: its fault, stall, open question or wait, and its latest progress. */
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
/** The phase and how long it has run, e.g. "build 42 min". */
export function assignmentPhase(a: AssignmentView, now = Date.now()): string {
  const phase = a.error ? "faulted" : (a.phase ?? a.status);
  return a.phaseSince === null
    ? phase
    : `${phase} ${Math.max(0, Math.round((now - a.phaseSince) / 60_000))} min`;
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
  "Tasks",
  "Agents",
  "Landing",
  "Full check",
  "Benchmarks",
  "Conformance",
  "Plan",
  "Machine",
  "Health",
  "Needs you",
] as const;
export type Tab = (typeof tabs)[number];
export const tabKey = (index: number) => (index < 9 ? String(index + 1) : index === 9 ? "0" : "");
