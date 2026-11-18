// nts's own part of the app's data: its tabs. The protocol the host and clients share
// (tasks, agents, Needs you, settings, notifications) comes from the kit.
export * from "@signalbox/kit/shared/protocol";

export const tabs = [
  "Now",
  // Second: what waits on the owner is one key away from anywhere.
  "Needs you",
  "Tasks",
  "Agents",
  "Landing",
  "Full check",
  "Benchmarks",
  "Conformance",
  "Plan",
  "Machine",
  "Health",
] as const;
export type Tab = (typeof tabs)[number];
export const tabKey = (index: number) => (index < 9 ? String(index + 1) : index === 9 ? "0" : "");
