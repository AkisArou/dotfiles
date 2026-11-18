// nts's wake-ups: a landing answer that needs a look and a full-check culprit,
// each with a stable ID its source owns.
import { test } from "node:test";
import assert from "node:assert/strict";
import { sourceOf } from "@signalbox/kit/orchestration/wakeups";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { capability } from "signalbox/core";
import { culpritEvents, landingEvents, ntsWakeups } from "./wakeup-sources.ts";

const landed = (id: string) =>
  ({
    service: { running: true, state: "idle", since: "", message: null },
    checking: null,
    waiting: [],
    held: [],
    day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null },
    recent: [
      {
        id,
        sha: id.padEnd(40, "0"),
        state: "refused",
        at: "",
        message: "red",
        branch: null,
        issue: 7,
        run: null,
      },
    ],
    main: { commit: null, pushed: null },
  }) as unknown as LandingStatus;

test("a refused landing and a culprit are events of their own sources", () => {
  const sources = ntsWakeups({
    landing: capability("landing"),
    fullCheck: capability("full-check"),
  });
  const [refused] = landingEvents(landed("abc"));
  assert.equal(refused!.id, "landing/abc/refused");
  assert.match(refused!.text, /Landing refused: #7 abc0{9}: red/);
  const [culprit] = culpritEvents({
    culprits: [
      { commit: "c".repeat(40), part: "core", steps: ["tooling"], revert: null, run: "/runs/x" },
    ],
  } as unknown as FullCheckStatus);
  assert.equal(culprit!.id, `culprit/${"c".repeat(40)}/core/open`);
  assert.equal(sourceOf(refused!.id, sources), "landing");
  assert.equal(sourceOf(culprit!.id, sources), "fullCheck");
  assert.equal(sourceOf("decided/q1", sources), "decisions");
});
