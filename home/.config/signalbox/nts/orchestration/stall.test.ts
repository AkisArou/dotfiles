import { test } from "node:test";
import assert from "node:assert/strict";
import { STALL, progressed, stallStep } from "./stall.ts";
import type { Watch } from "./stall.ts";

test("an idle agent is nudged twice, then the coordinator is told once", () => {
  let watch: Watch | null = null;
  const at = (now: number, status: string | undefined) => {
    const step = stallStep({ key: "build/claude/a/0", status, watch, now });
    watch = step.watch;
    return step;
  };
  assert.equal(at(0, "busy").action, "none");
  assert.deepEqual(at(1_000, "idle"), { watch: watch, action: "none", retryAt: 1_000 + STALL.idle });
  assert.equal(at(1_000 + STALL.idle - 1, "idle").action, "none");
  assert.equal(at(1_000 + STALL.idle, "idle").action, "nudge");
  assert.equal(at(1_000 + STALL.idle + 1, "idle").action, "none", "the timer restarts at a nudge");
  assert.equal(at(1_000 + 2 * STALL.idle, "idle").action, "nudge");
  assert.equal(at(1_000 + 3 * STALL.idle, "idle").action, "escalate");
  assert.equal(at(1_000 + 4 * STALL.idle, "idle").action, "none", "told once");
  assert.equal(watch!.nudges, STALL.nudges);
});

test("busy resets the idle timer; unknown status proves nothing; a new key is a new watch", () => {
  let step = stallStep({ key: "k", status: "idle", watch: null, now: 0 });
  step = stallStep({ key: "k", status: "busy", watch: step.watch, now: STALL.idle - 1 });
  assert.equal(step.watch.idleSince, null);
  step = stallStep({ key: "k", status: "idle", watch: step.watch, now: STALL.idle });
  assert.equal(step.action, "none", "idle counts from its own start");
  step = stallStep({ key: "k", status: undefined, watch: step.watch, now: 10 * STALL.idle });
  assert.equal(step.action, "none");
  const spent = { key: "k", idleSince: 0, until: null, nudges: STALL.nudges, escalated: true };
  assert.equal(stallStep({ key: "k2", status: "idle", watch: spent, now: 0 }).watch.nudges, 0);
});

test("reported progress holds nudges until its time, then counts idle from there", () => {
  const until = 60 * 60_000;
  let step = stallStep({ key: "k", status: "idle", watch: progressed("k", until), now: 0 });
  assert.deepEqual([step.action, step.retryAt], ["none", until]);
  step = stallStep({ key: "k", status: "idle", watch: step.watch, now: until - 1 });
  assert.equal(step.action, "none");
  step = stallStep({ key: "k", status: "idle", watch: step.watch, now: until });
  assert.equal(step.action, "nudge");
});
