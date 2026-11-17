// When a task waits on an agent's report, an agent that sits idle without
// reporting is stalled: most often it ended its turn without calling its tool.
// Idle is also how an agent waits for its own background work, so an agent says
// what it waits for (report_progress) and is left alone until then. Otherwise
// the route nudges it, and after the nudges are spent the coordinator decides.
import { z } from "zod";

export const STALL = {
  /** Idle this long, past any reported progress, before a nudge. */
  idle: 10 * 60_000,
  /** Nudges per awaited report before the coordinator is told. */
  nudges: 2,
} as const;

export const watchState = z.object({
  /** The awaited report: phase, agent and round. A new key starts a new watch. */
  key: z.string(),
  idleSince: z.number().nullable(),
  /** The agent's own estimate: idle before this time is expected. */
  until: z.number().nullable(),
  nudges: z.number().int().nonnegative(),
  escalated: z.boolean(),
});
export type Watch = z.infer<typeof watchState>;

export type StallStep = {
  readonly watch: Watch;
  readonly action: "none" | "nudge" | "escalate";
  /** When to look again if nothing else changes. */
  readonly retryAt?: number;
};

/** One observation of the awaited agent. `status` is Claude Code's (busy, idle) or unknown. */
export function stallStep(input: {
  readonly key: string;
  readonly status: string | undefined;
  readonly watch: Watch | null;
  readonly now: number;
}): StallStep {
  const watch: Watch =
    input.watch?.key === input.key
      ? input.watch
      : { key: input.key, idleSince: null, until: null, nudges: 0, escalated: false };
  // Busy is working; an unknown status (a Codex agent, a failed reading) proves nothing.
  if (input.status !== "idle") return { watch: { ...watch, idleSince: null }, action: "none" };
  const idleSince = watch.idleSince ?? input.now;
  const due = Math.max(idleSince + STALL.idle, watch.until ?? 0);
  if (input.now < due) return { watch: { ...watch, idleSince }, action: "none", retryAt: due };
  if (watch.nudges < STALL.nudges)
    return {
      watch: { ...watch, idleSince: input.now, nudges: watch.nudges + 1 },
      action: "nudge",
      retryAt: input.now + STALL.idle,
    };
  if (!watch.escalated)
    return { watch: { ...watch, idleSince, escalated: true }, action: "escalate" };
  return { watch: { ...watch, idleSince }, action: "none" };
}

/** The agent reported what it waits for: expected idle until then, and a fresh set of nudges. */
export function progressed(key: string, until: number): Watch {
  return { key, idleSince: null, until, nudges: 0, escalated: false };
}
