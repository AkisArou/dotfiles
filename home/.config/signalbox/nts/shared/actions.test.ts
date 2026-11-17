import { test } from "node:test";
import assert from "node:assert/strict";
import { earlierTasks, taskActions } from "./actions.ts";
import type { AssignmentView, Issue } from "./model.ts";

const issue = (labels: string[]) => ({ number: 7, labels }) as unknown as Issue;
const task = (change: Partial<AssignmentView>) =>
  ({
    id: "a",
    issue: 7,
    status: "active",
    phase: "build",
    error: null,
    worker: "w",
    acceptedAt: 1,
    ...change,
  }) as AssignmentView;
const ids = (actions: readonly { id: string }[]) => actions.map((action) => action.id);

test("a task offers only the actions its state allows", () => {
  assert.deepEqual(ids(taskActions(issue(["task"]), undefined)), ["assign", "prioritize"]);
  assert.deepEqual(ids(taskActions(issue(["kind:group"]), undefined)), [], "not a task: nothing");
  assert.deepEqual(ids(taskActions(issue(["task"]), task({ phase: "approve" }))), [
    "approve",
    "return",
    "message",
    "prioritize",
    "cancel",
  ]);
  assert.deepEqual(ids(taskActions(issue(["task"]), task({ phase: "signoff" }))), [
    "signoff",
    "refuse",
    "message",
    "prioritize",
    "cancel",
  ]);
  assert.deepEqual(ids(taskActions(issue(["task"]), task({ phase: "verify", worker: null }))), [
    "rework",
    "prioritize",
    "cancel",
  ]);
  assert.deepEqual(
    ids(taskActions(issue(["task"]), task({ phase: "workspace", worker: null, error: "boom" }))),
    ["retry", "prioritize", "cancel"],
  );
});

test("an issue's earlier tasks are its finished and cancelled ones, newest first", () => {
  const all = [
    task({ id: "old", status: "cancelled", acceptedAt: 1 }),
    task({ id: "live", status: "active", acceptedAt: 3 }),
    task({ id: "newer", status: "finished", acceptedAt: 2 }),
    task({ id: "other", status: "finished", issue: 8, acceptedAt: 4 }),
  ];
  assert.deepEqual(ids(earlierTasks(all, 7)), ["newer", "old"]);
});
