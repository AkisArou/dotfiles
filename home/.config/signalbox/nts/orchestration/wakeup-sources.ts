// nts's own wake-ups for the coordinator, beside the kit's: each landing answer
// that needs a look (a refusal, a hold, a withdrawal, or a landing that names no
// issue) and each full-check culprit. Their names keep the marks of what was
// already announced (seeded/landing, seeded/fullCheck).
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
  ];
}
