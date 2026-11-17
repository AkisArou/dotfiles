// What you can do to a task, the same in the TUI and the web: each action is
// offered only where the task's state allows it, says what it does, and says
// what it needs from you (nothing, a note, words, or an urgency).
import type { AssignmentView, Issue } from "./model.ts";
import type { Project } from "./use-project.ts";

export type TaskActionId =
  | "assign"
  | "approve"
  | "return"
  | "signoff"
  | "refuse"
  | "rework"
  | "retry"
  | "message"
  | "prioritize"
  | "cancel";

/** none: runs at once · optional: a note you may leave empty · required: words · urgency: -9..9. */
export type ActionInput = "none" | "optional" | "required" | "urgency";

export type TaskAction = {
  readonly id: TaskActionId;
  readonly label: string;
  readonly meaning: string;
  readonly input: ActionInput;
};

const ACTIONS: Record<TaskActionId, TaskAction> = {
  assign: {
    id: "assign",
    label: "Start the task",
    meaning: "queue it for a worker now, with a note for the worker if you like",
    input: "optional",
  },
  approve: {
    id: "approve",
    label: "Approve the design",
    meaning: "the worker builds it (your notes go with the approval)",
    input: "optional",
  },
  return: {
    id: "return",
    label: "Send the design back",
    meaning: "the worker revises it from your notes",
    input: "required",
  },
  signoff: {
    id: "signoff",
    label: "Sign off the change",
    meaning: "a reviewed core change goes to the landing check",
    input: "optional",
  },
  refuse: {
    id: "refuse",
    label: "Refuse the change",
    meaning: "back to build with your notes",
    input: "required",
  },
  rework: {
    id: "rework",
    label: "Send the landing back",
    meaning: "a held or wrong landing returns to build with your notes",
    input: "required",
  },
  retry: { id: "retry", label: "Retry", meaning: "run the faulted step again", input: "none" },
  message: {
    id: "message",
    label: "Message the worker",
    meaning: "your words reach its session (answers a question, steers the work)",
    input: "required",
  },
  prioritize: {
    id: "prioritize",
    label: "Set its urgency",
    meaning: "-9..9, higher first within its share of the workers (0 is the default)",
    input: "urgency",
  },
  cancel: {
    id: "cancel",
    label: "Cancel the task",
    meaning: "its sessions end; its tree and branch stay with the pool",
    input: "required",
  },
};

/** The actions a task's state allows, in the order you would want them. */
export function taskActions(issue: Issue, task: AssignmentView | undefined): TaskAction[] {
  if (!task) return issue.labels.includes("task") ? [ACTIONS.assign, ACTIONS.prioritize] : [];
  const phase = task.phase ?? "";
  return [
    ...(task.error ? [ACTIONS.retry] : []),
    ...(phase === "approve" ? [ACTIONS.approve, ACTIONS.return] : []),
    ...(phase === "signoff" ? [ACTIONS.signoff, ACTIONS.refuse] : []),
    ...(phase === "landing" || phase === "verify" ? [ACTIONS.rework] : []),
    ...(task.worker ? [ACTIONS.message] : []),
    ACTIONS.prioritize,
    ACTIONS.cancel,
  ];
}

/** What you gave: words (or none), an urgency, and for assign the model when workers are named. */
export type ActionValues = { text?: string; urgency?: number; model?: string };

/** Runs an action through the host's operations. */
export function runTaskAction(
  commands: Project["commands"],
  action: TaskActionId,
  issue: number,
  task: AssignmentView | undefined,
  values: ActionValues,
): Promise<unknown> {
  const text = values.text?.trim() ?? "";
  const assignment = task?.id ?? "";
  switch (action) {
    case "assign":
      return commands.assignTask.run({
        issue,
        instructions: text,
        ...(values.model ? { model: values.model } : {}),
      });
    case "approve":
    case "return":
      return commands.decideDesign.run({ assignment, accept: action === "approve", notes: text });
    case "signoff":
    case "refuse":
      return commands.signOff.run({ assignment, accept: action === "signoff", notes: text });
    case "rework":
      return commands.reworkAssignment.run({ assignment, notes: text });
    case "retry":
      return commands.retryAssignment.run({ assignment });
    case "message":
      return commands.messageWorker.run({ assignment, text });
    case "prioritize":
      return commands.prioritizeTask.run({ issue, urgency: values.urgency ?? 0 });
    case "cancel":
      return commands.cancelAssignment.run({ assignment, reason: text });
  }
}

/** An issue's earlier tasks (finished or cancelled), newest first. */
export function earlierTasks(
  assignments: readonly AssignmentView[],
  issue: number,
): AssignmentView[] {
  return assignments
    .filter((a) => a.issue === issue && a.status !== "queued" && a.status !== "active")
    .sort((a, b) => b.acceptedAt - a.acceptedAt);
}
