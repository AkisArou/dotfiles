// The tones the TUI and the web colour states by: one rule for both, so a busy
// agent, a held issue or a faulted task reads the same in either.
import { test } from "node:test";
import assert from "node:assert/strict";
import type { Agent, AssignmentView, Issue } from "./model.ts";
import {
  agentLines,
  agentTone,
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
    ["held", "parked", "needs-user", "unscoped", "ready", "in_progress", "group"].map(issueTone),
    ["attention", "attention", "attention", "attention", "plain", "plain", "plain"],
  );
  const task = (change: Partial<AssignmentView>) =>
    assignmentTone({
      status: "active",
      error: null,
      escalation: null,
      stalled: null,
      help: null,
      ...change,
    } as AssignmentView);
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
