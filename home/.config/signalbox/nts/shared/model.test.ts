// The tones the TUI and the web colour states by: one rule for both, so a busy
// agent, a held issue or a faulted task reads the same in either.
import { test } from "node:test";
import assert from "node:assert/strict";
import type { Agent, AssignmentView, Issue } from "./model.ts";
import {
  agentLines,
  agentTone,
  liveTask,
  taskWord,
  assignmentTone,
  gaugeTone,
  issueTone,
  postedTone,
} from "./model.ts";

test("states read the same in the TUI and the web: one tone each", () => {
  assert.deepEqual(
    ["busy", "running", "idle", "external", "ended", "exited", "stopped"].map(agentTone),
    ["good", "good", "plain", "plain", "quiet", "quiet", "attention"],
    "busy works, idle waits, ended is gone, an unknown state wants a look",
  );
  assert.deepEqual(
    ["held", "parked", "needs-user", "unscoped", "orphaned", "ready", "in_progress", "group"].map(
      issueTone,
    ),
    ["attention", "attention", "attention", "attention", "attention", "plain", "plain", "plain"],
  );
  const task = (change: Partial<AssignmentView>) => assignmentTone(view(change));
  assert.equal(task({}), "plain");
  assert.equal(task({ error: "boom" }), "bad");
  assert.equal(task({ escalation: "decide" }), "attention");
  assert.equal(task({ stalled: "idle" }), "attention");
  assert.equal(task({ help: "a question" }), "attention");
  assert.equal(task({ status: "finished" }), "quiet");
  assert.deepEqual((["red", "amber", "ok"] as const).map(gaugeTone), ["bad", "attention", "good"]);
  assert.deepEqual(["done", "waiting", "HTTP 502"].map(postedTone), ["quiet", "active", "bad"]);
});

test("an agent's lines: the issue it works on by title, then its role and tree", () => {
  const agent = (change: Partial<Agent>): Agent => ({
    id: "a",
    name: "#823 worker",
    role: "worker",
    state: "busy",
    path: "/home/me/.cache/nts-pool/7",
    issue: 823,
    ...change,
  });
  const issues = [{ number: 823, title: "Withdraw an aborted token wait" }] as Issue[];
  assert.deepEqual(agentLines(agent({}), issues), [
    "#823 Withdraw an aborted token wait",
    "worker · /home/me/.cache/nts-pool/7",
  ]);
  assert.deepEqual(
    agentLines(agent({}), issues, (tree) => tree.replace("/home/me", "~")),
    ["#823 Withdraw an aborted token wait", "worker · ~/.cache/nts-pool/7"],
    "the tree is shortened as the caller shows it",
  );
  assert.deepEqual(agentLines(agent({ issue: 900 }), issues), [
    "#900",
    "worker · /home/me/.cache/nts-pool/7",
  ]);
  assert.deepEqual(agentLines(agent({ issue: null, role: "coordinator", path: null }), issues), [
    "coordinator · native terminal",
  ]);
});

function view(change: Partial<AssignmentView>): AssignmentView {
  return {
    status: "active",
    phase: "build",
    error: null,
    escalation: null,
    stalled: null,
    help: null,
    waitingOn: [],
    ahead: null,
    unordered: false,
    issue: 1,
    acceptedAt: 0,
    ...change,
  } as AssignmentView;
}

test("a task's word for lists: its place in the worker queue, its phase, or what it needs", () => {
  assert.equal(
    taskWord(view({ phase: "workspace", waitingOn: ["resource:worker"] })),
    "next for a worker",
  );
  assert.equal(
    taskWord(view({ phase: "workspace", waitingOn: ["resource:worker"], ahead: "issue/828" })),
    "queued after #828",
  );
  assert.equal(
    taskWord(view({ status: "queued", phase: "prepare", waitingOn: ["route:nts/task"] })),
    "queued",
  );
  assert.equal(taskWord(view({})), "building");
  assert.equal(
    taskWord(view({ phase: "workspace", waitingOn: ["resource:worker"], unordered: true })),
    "queued",
    "a place not known yet is not next",
  );
  assert.equal(taskWord(view({ help: "a question" })), "asks");
  assert.equal(taskWord(view({ error: "boom", waitingOn: ["resource:worker"] })), "faulted");
  const tasks = [
    view({ issue: 7, status: "finished", acceptedAt: 3 }),
    view({ issue: 7, status: "active", acceptedAt: 2, phase: "verify" }),
    view({ issue: 8, status: "queued", acceptedAt: 1 }),
  ];
  assert.equal(liveTask(tasks, 7)?.phase, "verify", "a finished task is not live");
  assert.equal(liveTask(tasks, 9), undefined);
});
