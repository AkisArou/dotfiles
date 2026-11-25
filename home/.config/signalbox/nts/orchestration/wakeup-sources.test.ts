// nts's wake-ups: a landing answer that needs a look and a full-check culprit,
// each with a stable ID its source owns.
import { test } from "node:test";
import assert from "node:assert/strict";
import { sourceOf } from "@signalbox/kit/orchestration/wakeups";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import { capability } from "signalbox/core";
import { benchmarkEvents, culpritEvents, landingEvents, ntsWakeups } from "./wakeup-sources.ts";

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

const night = (last: object, slower: string[] = []) =>
  ({ benchmarks: { last, slower, due: null } }) as unknown as FullCheckStatus;

test("a night's benchmarks wake the coordinator once they are over", () => {
  const sources = ntsWakeups({
    landing: capability("landing"),
    fullCheck: capability("full-check"),
  });
  const base = { commit: "d".repeat(40), at: "2026-11-24T20:49:24.560Z", measured: true };
  const [landed] = benchmarkEvents(
    night(
      {
        ...base,
        outcome: "landed",
        result: "landed as 397efd49a13c",
        record: "/runs/b/2026-11-24-dddd-c37.json",
      },
      ["parse-int-default"],
    ),
  );
  assert.equal(landed!.id, "benchmarks/2026-11-24-dddd-c37/landed");
  assert.match(landed!.text, /Slower than their own best: parse-int-default\./);
  assert.equal(sourceOf(landed!.id, sources), "benchmarks");
  const [none] = benchmarkEvents(
    night({ ...base, outcome: "landed", result: "x", record: "/r/a.json" }),
  );
  assert.match(none!.text, /none flagged/);

  const [missed] = benchmarkEvents(
    night({
      ...base,
      measured: false,
      outcome: "not-run",
      result: "not run: the build at verified is red",
      run: "/runs/b",
    }),
  );
  assert.equal(missed!.id, `benchmarks/${"d".repeat(12)}/${base.at}/not-run`);
  assert.match(
    missed!.text,
    /without a record on main \(not-run\): not run: the build at verified is red\nLogs: \/runs\/b/,
  );

  // A night still measuring, or publishing (failed publications are retried), says nothing yet.
  for (const outcome of [undefined, "record", "submitted", "failed", "nothing"])
    assert.deepEqual(benchmarkEvents(night({ ...base, outcome, result: "" })), []);
  assert.deepEqual(
    benchmarkEvents({
      benchmarks: { last: null, slower: [], due: null },
    } as unknown as FullCheckStatus),
    [],
  );
});
