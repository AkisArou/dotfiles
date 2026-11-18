// Every status word the UIs can show has a word and a meaning in the glossary:
// a new phase or state cannot reach a list unexplained.
import { test } from "node:test";
import assert from "node:assert/strict";
import { phases } from "../orchestration/task-model.ts";
import { agentStates, issueStates, taskConditions, taskPhases } from "@signalbox/kit/shared/glossary";

test("the glossary names and explains every phase, issue state and agent state", () => {
  for (const phase of phases) assert.ok(taskPhases[phase]?.meaning, `phase ${phase}`);
  // The states the host's projection (host/tasks.ts) and the observed claims (claims.ts) give.
  for (const state of [
    "ready",
    "unscoped",
    "in_progress",
    "orphaned",
    "held",
    "parked",
    "needs-user",
    "group",
  ])
    assert.ok(issueStates[state]?.meaning, `issue state ${state}`);
  for (const state of ["busy", "running", "idle", "external", "ended", "exited"])
    assert.ok(agentStates[state]?.meaning, `agent state ${state}`);
  for (const term of [...Object.values(taskConditions), ...Object.values(taskPhases)])
    assert.ok(term.label && term.meaning, term.label);
});
