// Every status word the TUI and the web show, with what it means: one place, so
// the lists, the filters, the tooltips and the help say the same thing. Label
// meanings (kind:, area:, state:, task) are NTS's own, in .github/labels.json,
// served by the host (refs.labels); this file holds the words Signalbox derives.

export type Term = { readonly label: string; readonly meaning: string };

/** An issue's state, from its labels and from the work observed on it. */
export const issueStates: Record<string, Term> = {
  ready: {
    label: "ready",
    meaning: "a task (the task label) that nothing holds: it can be queued",
  },
  in_progress: {
    label: "in progress",
    meaning:
      "being worked on: an agent in its tree, a tree used within the hour, or the landing service has it",
  },
  orphaned: {
    label: "orphaned",
    meaning:
      "a pool tree holds work for it, but nobody has used the tree for an hour and no task is queued",
  },
  unscoped: {
    label: "unscoped",
    meaning: "no task label yet: it must be scoped as a task before it can be queued",
  },
  held: { label: "held", meaning: "waits on something the issue names (state:held or hold:all)" },
  parked: {
    label: "parked",
    meaning: "waits on a decision, a licence or your word (state:parked)",
  },
  "needs-user": { label: "needs you", meaning: "waits on your answer (state:needs-user)" },
  group: { label: "group", meaning: "a group of tasks (kind:group), not a task itself" },
};

/** A task's phase, from accepted to done, in the order it moves. */
export const taskPhases: Record<string, Term> = {
  prepare: { label: "preparing", meaning: "reading the issue and the paths its card names" },
  workspace: {
    label: "taking a tree",
    meaning: "taking a pool tree once a worker slot is free",
  },
  start: { label: "starting", meaning: "starting the worker's session in its tree" },
  design: { label: "designing", meaning: "the worker writes its design" },
  approve: { label: "design to approve", meaning: "the coordinator reads and approves the design" },
  build: { label: "building", meaning: "the worker builds, tests and proves the change" },
  review: { label: "starting review", meaning: "starting a fresh reviewer for the exact commit" },
  reviewing: { label: "in review", meaning: "a fresh reviewer checks the exact commit" },
  signoff: {
    label: "sign-off",
    meaning:
      "a change to the compiler core, runtime/c, the gate or the landing tools waits for the coordinator to read it",
  },
  escalated: {
    label: "escalated",
    meaning: "the coordinator decides: too many rounds, or a disputed design",
  },
  queue: { label: "submitting", meaning: "submitting the commit to the landing check, once" },
  landing: { label: "landing", meaning: "the landing check runs it against the newest main" },
  verify: { label: "verifying", meaning: "landed; the full check verifies it after landing" },
  retire: { label: "wrapping up", meaning: "ending the task's sessions" },
  done: { label: "done", meaning: "finished" },
};

/** What a task's word says when it is not its phase. */
export const taskConditions: Record<string, Term> = {
  next: {
    label: "next for a worker",
    meaning: "first in the worker queue: it starts when a slot frees",
  },
  after: {
    label: "queued after #N",
    meaning: "in the worker queue behind that task (the shares rank it)",
  },
  queued: {
    label: "queued",
    meaning: "accepted and waiting; its place is decided once the host has read the issues",
  },
  asks: { label: "asks", meaning: "its worker asked a question; the coordinator answers" },
  stalled: { label: "stalled", meaning: "its agent stayed idle through the nudges" },
  escalated: taskPhases.escalated!,
  faulted: { label: "faulted", meaning: "it hit an error; its detail says which" },
  stranded: {
    label: "stranded",
    meaning:
      "the task route changed under it and cannot run it: cancel it (its cleanup is then yours), or retry once a route that fits is back",
  },
};

/** An agent session's state. */
export const agentStates: Record<string, Term> = {
  busy: { label: "busy", meaning: "working on a turn" },
  running: { label: "running", meaning: "its program runs (a native terminal)" },
  idle: { label: "idle", meaning: "waits for a message" },
  external: {
    label: "external",
    meaning: "a session outside Signalbox, seen in your tmux; its activity is not known",
  },
  ended: { label: "ended", meaning: "the session is gone" },
  exited: { label: "exited", meaning: "its program exited" },
};

/** The words for the assigned facet. */
export const assignedValues: Record<string, Term> = {
  yes: {
    label: "yes",
    meaning: "an owner: an agent: label, a GitHub assignee, or an agent in its tree",
  },
  no: { label: "no", meaning: "nobody owns it" },
};
