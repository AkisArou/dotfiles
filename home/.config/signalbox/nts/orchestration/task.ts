// The NTS task route: one GitHub issue, one fresh worker, NTS's own gates.
// Phases follow docs/workflow.md (task-phases.ts); the data is task-model.ts.
import { latestValue, mapReadable } from "signalbox/core";
import { assignmentRoute } from "signalbox/ext/assignments/index";
import { cancelTask, interrupts, phaseHandlers, turn } from "./task-phases.ts";
import type { TaskOptions } from "./task-phases.ts";
import { initialTaskState, taskInput, taskReport, taskState } from "./task-model.ts";
import type { Phase } from "./task-model.ts";

const WITHOUT_WORKER: readonly Phase[] = ["prepare", "verify", "retire", "done"];

/** Labels that keep an issue from starting: the user's, or the plan's. */
const BLOCKING = ["state:needs-user", "state:held", "state:parked"];

export function taskRoute(options: Omit<TaskOptions, "clock"> & { readonly clock?: () => number }) {
  const settings: TaskOptions = { ...options, clock: options.clock ?? Date.now };
  return assignmentRoute({
    id: "nts/task",
    // Bump with any change to state or phase meaning; old assignments then fault
    // visibly instead of being reinterpreted (keep the old revision until they drain).
    revision: "1",
    input: taskInput,
    concurrency: 4,
    resources: (input, state) => [
      { resource: `issue/${input.issue}`, units: 1 },
      // A worker slot is held while an agent works for the task; waiting for the
      // full check to verify a landed change holds none.
      ...(WITHOUT_WORKER.includes(state.phase) ? [] : [{ resource: "worker", units: 1 }]),
      ...state.paths.map((path) => ({ resource: `file:${path}`, units: 1 })),
    ],
    admission: [
      {
        requires: [options.issues],
        watch: (ctx) => [ctx.get(options.issues)],
        check(ctx) {
          const issues = latestValue(ctx.get(options.issues));
          if (!issues) return { allowed: false, reason: "Issues are not read yet" };
          const found = issues.find((issue) => issue.number === ctx.input.issue);
          if (!found)
            return { allowed: false, reason: `Issue #${ctx.input.issue} is not an open task` };
          const blocked = found.labels.find((label) => BLOCKING.includes(label));
          if (blocked) return { allowed: false, reason: `Issue #${ctx.input.issue} is ${blocked}` };
          if (!found.labels.includes("task"))
            return { allowed: false, reason: `Issue #${ctx.input.issue} is not a task issue` };
          return { allowed: true };
        },
      },
    ],
    lifecycle: {
      requires: [
        options.issues,
        options.landing,
        options.fullCheck,
        options.profiles,
        options.agents.runtime,
      ],
      // Each wake is an activation: watch only what the phases decide on, so a
      // status's timestamps and counters changing every poll wake nothing.
      watch: (ctx) => [
        mapReadable(ctx.get(options.landing), (status) => ({
          main: status.main.commit,
          checking: status.checking?.sha ?? null,
          waiting: status.waiting.map((entry) => entry.sha),
          held: status.held.map((entry) => entry.sha),
          answers: status.recent.map((answer) => `${answer.id}:${answer.state}`),
        })),
        mapReadable(ctx.get(options.fullCheck), (status) => status.verified.commit),
        mapReadable(ctx.get(options.agents.runtime).instances, (instances) =>
          instances.map((instance) => `${instance.id}:${instance.state}`),
        ),
      ],
      state: taskState,
      report: taskReport,
      initial: initialTaskState,
      async reconcile(ctx) {
        const t = turn(ctx, settings);
        for (const interrupt of interrupts) {
          const step = await interrupt(t);
          if (step) return step;
        }
        return phaseHandlers[t.state.phase](t);
      },
      cancel: (ctx) => cancelTask(ctx, settings),
    },
  });
}
export type TaskRoute = ReturnType<typeof taskRoute>;
