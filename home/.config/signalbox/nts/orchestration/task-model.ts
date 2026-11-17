// The NTS task's data: its input, the reports agents and people send it, its
// phases and its durable state. The route (task.ts) and its phases
// (task-phases.ts) act on it; tools, wake-ups and views read it.
import { z } from "zod";
import { profileSchema } from "./profiles.ts";
import { watchState } from "./stall.ts";

const sha = z.string().regex(/^[0-9a-f]{40}$/);

export const taskInput = z.object({
  issue: z.number().int().positive(),
  /** The coordinator's note for this task: scope, order, things to watch. */
  instructions: z.string().default(""),
});
export type TaskInput = z.infer<typeof taskInput>;

export const taskReport = z.discriminatedUnion("kind", [
  z.object({ kind: z.literal("design"), summary: z.string().min(1) }),
  z.object({ kind: z.literal("design_decision"), accept: z.boolean(), notes: z.string() }),
  /** The coordinator's (or owner's) reading of a core change before it is submitted. */
  z.object({ kind: z.literal("signoff"), accept: z.boolean(), notes: z.string() }),
  z.object({ kind: z.literal("ready"), commit: sha, summary: z.string().min(1) }),
  z.object({
    kind: z.literal("review"),
    commit: sha,
    verdict: z.enum(["accept", "changes"]),
    findings: z.string(),
    /** The reviewer disputes the approved design itself; the coordinator decides. */
    disputesDesign: z.boolean().default(false),
  }),
  z.object({ kind: z.literal("help"), message: z.string().min(1) }),
  /** The coordinator (or user) sends a held or landed-but-wrong change back to build. */
  z.object({ kind: z.literal("rework"), notes: z.string().min(1) }),
  /** An agent says what it waits for and for how long; idle until then is expected. */
  z.object({
    kind: z.literal("progress"),
    role: z.enum(["worker", "reviewer"]),
    note: z.string().min(1),
    minutes: z.number().int().min(1).max(480),
  }),
  /** A message to the worker: delivered once; it answers open help and decides an escalation. */
  z.object({
    kind: z.literal("message"),
    from: z.enum(["coordinator", "user"]),
    text: z.string().min(1),
  }),
]);
export type TaskReport = z.infer<typeof taskReport>;
export type TaskReportOf<K extends TaskReport["kind"]> = Extract<TaskReport, { kind: K }>;

export const phases = [
  "prepare",
  "workspace",
  "start",
  "design",
  "approve",
  "build",
  "review",
  "reviewing",
  /** A reviewed change to the compiler core, runtime/c, the gate or the landing
   * tools waits for the coordinator to read it before it is submitted (AGENTS.md). */
  "signoff",
  /** Waits for the coordinator: too many rounds, or a disputed design. */
  "escalated",
  "queue",
  "landing",
  "verify",
  "retire",
  "done",
] as const;
export type Phase = (typeof phases)[number];

export const taskState = z.object({
  phase: z.enum(phases),
  title: z.string(),
  revision: z.string(),
  paths: z.array(z.string()),
  tree: z.string().nullable(),
  branch: z.string().nullable(),
  worker: z.object({ id: z.string(), profile: profileSchema }).nullable(),
  reviewer: z
    .object({ id: z.string(), commit: sha, round: z.number().int(), profile: profileSchema })
    .nullable(),
  design: z.string(),
  commit: sha.nullable(),
  summary: z.string(),
  /** Returns to build: review findings and landing refusals together. */
  rounds: z.number().int().nonnegative(),
  landedMain: sha.nullable(),
  /** The worker's open question for the coordinator. */
  help: z.string().nullable(),
  outcome: z.string().nullable(),
  /** Worker launches so far; a replacement after a proven-ended session is a new generation. */
  // Defaults keep state written before these fields readable; a pre-existing
  // assignment counts as generation 1, so its replacement gets a fresh launch key.
  generation: z.number().int().nonnegative().default(1),
  /** The phase a replacement worker resumes, and the latest instruction it must see. */
  resumeTo: z.enum(phases).nullable().default(null),
  /** The latest instruction to the worker; while escalated, the one awaiting a decision. */
  note: z.string().default(""),
  /** Every earlier review round's findings, so each fresh reviewer checks them first. */
  reviews: z.array(z.object({ commit: sha, findings: z.string() })).default([]),
  /** When this round queued its commit; only landing answers after it count. */
  queuedAt: z.number().nullable().default(null),
  /** When the current phase began (ms); the coordinator hears of a phase past its budget. */
  phaseSince: z.number().nullable().default(null),
  /** The awaited agent's idle watch (stall.ts). */
  watch: watchState.nullable().default(null),
  /** The awaited agent's latest progress note. */
  progress: z
    .object({ note: z.string(), at: z.number(), until: z.number() })
    .nullable()
    .default(null),
  /** Set when an awaited agent stayed idle through its nudges; the coordinator decides. */
  stalled: z.string().nullable().default(null),
  /** What the coordinator must decide while the task is escalated. */
  escalation: z.string().nullable().default(null),
  /** The exact commit a review accepted; reporting it again (after an innocent refusal) needs no new review. */
  accepted: sha.nullable().default(null),
  /** The core paths the accepted commit changes; non-empty means it needs a sign-off. */
  core: z.array(z.string()).default([]),
  /** The exact commit the coordinator signed off. */
  signedOff: sha.nullable().default(null),
});
export type TaskState = z.infer<typeof taskState>;

export const initialTaskState = (): TaskState => ({
  phase: "prepare",
  title: "",
  revision: "",
  paths: [],
  tree: null,
  branch: null,
  worker: null,
  reviewer: null,
  design: "",
  commit: null,
  summary: "",
  rounds: 0,
  landedMain: null,
  help: null,
  outcome: null,
  generation: 0,
  resumeTo: null,
  note: "",
  reviews: [],
  queuedAt: null,
  phaseSince: null,
  watch: null,
  progress: null,
  stalled: null,
  escalation: null,
  accepted: null,
  core: [],
  signedOff: null,
});

/** Paths named on the card's **Paths:** line, as resources two tasks must not share. */
export function cardPaths(body: string): string[] {
  const line = body.split("\n").find((text) => /^\*\*Paths:\*\*/.test(text.trim()));
  if (!line) return [];
  const tokens = line.replace(/^\s*\*\*Paths:\*\*/, "").match(/[\w.@-]+(?:\/[\w.@*{}-]+)+/g) ?? [];
  return [...new Set(tokens.map((token) => token.replace(/[.,;:]+$/, "")))].sort();
}

/** The agent whose report the task waits for, if any. A worker with an open question waits on the coordinator. */
export function awaitedAgent(
  state: TaskState,
): { role: "worker" | "reviewer"; id: string; key: string } | undefined {
  if ((state.phase === "design" || state.phase === "build") && state.worker && !state.help)
    return {
      role: "worker",
      id: state.worker.id,
      key: `${state.phase}/${state.worker.id}/${state.rounds}`,
    };
  if (state.phase === "reviewing" && state.reviewer)
    return {
      role: "reviewer",
      id: state.reviewer.id,
      key: `reviewing/${state.reviewer.id}/${state.reviewer.round}`,
    };
  return undefined;
}
