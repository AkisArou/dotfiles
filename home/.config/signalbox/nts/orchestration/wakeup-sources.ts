// nts's own wake-ups for the coordinator, beside the kit's: each landing answer
// that needs a look (a refusal, a hold, a withdrawal, or a landing that names no
// issue), each full-check culprit, and each night's benchmarks once they end:
// a run record landed on main, or a night that published none. Their names keep
// the marks of what was already announced (seeded/landing, seeded/fullCheck,
// seeded/benchmarks).
import { basename } from "node:path";
import { latestValue } from "signalbox/core";
import type { Capability, Readable } from "signalbox/core";
import type { Wakeup, WakeupSource } from "@signalbox/kit/orchestration/wakeups";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";

export function landingEvents(status: LandingStatus | undefined): Wakeup[] {
  return (status?.recent ?? [])
    .filter((answer) => answer.state !== "landed" || answer.issue !== null)
    .map((answer) => ({
      id: `landing/${answer.id}/${answer.state}`,
      text: `Landing ${answer.state}: ${answer.issue ? `#${answer.issue} ` : ""}${answer.sha.slice(0, 12)}: ${answer.message}${answer.run ? `\nLogs: ${answer.run}` : ""}`,
    }));
}

export function culpritEvents(status: FullCheckStatus | undefined): Wakeup[] {
  return (status?.culprits ?? []).map((culprit) => ({
    id: `culprit/${culprit.commit}/${culprit.part}/${culprit.revert ?? "open"}`,
    text: `Full check ${culprit.part} red: culprit ${culprit.commit.slice(0, 12)} (${culprit.steps.join(", ")})${culprit.revert ? `, reverted as ${culprit.revert.slice(0, 12)}` : ""}. Logs: ${culprit.run}`,
  }));
}

/** How a night ends without a record on main; a failed publication is tried again, so it is not one. */
const UNPUBLISHED = new Set(["no-record", "interrupted", "not-run", "given-up", "withdrawn"]);

/** A night's benchmarks, once they are over: its run record landed, or the night published none. */
export function benchmarkEvents(status: FullCheckStatus | undefined): Wakeup[] {
  const night = status?.benchmarks.last;
  if (!night?.outcome) return [];
  const commit = night.commit.slice(0, 12);
  if (night.outcome === "landed" && night.record) {
    const run = basename(night.record, ".json");
    const slower = status?.benchmarks.slower ?? [];
    return [
      {
        id: `benchmarks/${run}/landed`,
        text: `Nightly benchmarks landed: run ${run} at ${commit}, ${night.result}. Slower than their own best: ${slower.length ? slower.join(", ") : "none flagged"}. Results: results/benchmarks/full.md.`,
      },
    ];
  }
  if (!UNPUBLISHED.has(night.outcome)) return [];
  return [
    {
      id: `benchmarks/${commit}/${night.at}/${night.outcome}`,
      text: `Nightly benchmarks at ${commit} ended without a record on main (${night.outcome}): ${night.result}${night.run ? `\nLogs: ${night.run}` : ""}`,
    },
  ];
}

export function ntsWakeups(options: {
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
}): WakeupSource[] {
  return [
    {
      name: "landing",
      prefixes: ["landing/"],
      requires: [options.landing],
      status: (get) => get(options.landing).get().status,
      events: (get) => landingEvents(latestValue(get(options.landing))),
    },
    {
      name: "fullCheck",
      prefixes: ["culprit/"],
      requires: [options.fullCheck],
      status: (get) => get(options.fullCheck).get().status,
      events: (get) => culpritEvents(latestValue(get(options.fullCheck))),
    },
    {
      name: "benchmarks",
      prefixes: ["benchmarks/"],
      requires: [options.fullCheck],
      status: (get) => get(options.fullCheck).get().status,
      events: (get) => benchmarkEvents(latestValue(get(options.fullCheck))),
    },
  ];
}
