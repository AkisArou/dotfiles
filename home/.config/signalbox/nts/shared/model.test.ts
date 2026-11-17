// The tones the TUI and the web colour states by: one rule for both, so a busy
// agent, a held issue or a faulted task reads the same in either.
import { test } from "node:test";
import assert from "node:assert/strict";
import type { AssignmentView } from "./model.ts";
import { agentTone, assignmentTone, gaugeTone, issueTone, postedTone } from "./model.ts";

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
