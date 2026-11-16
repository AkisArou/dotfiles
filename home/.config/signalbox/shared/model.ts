// NTS application data shared by the host, web client and terminal client.
import { operation, source } from "signalbox/core/remote";

export type Issue = {
  id: string;
  number: number;
  title: string;
  body: string;
  url: string;
  area: string;
  stream: string;
  group: string;
  labels: string[];
  state: string;
  assignees: string[];
  owner: string | null;
  blockedBy: number[];
  workspace?: string;
  claims?: readonly import("../nts/model.ts").WorkClaim[];
  parent: { number: number; title: string } | null;
  progress: { total: number; completed: number } | null;
};
export type Agent = {
  id: string;
  name: string;
  role: string;
  state: string;
  path: string | null;
  issue: number | null;
  terminal?: string;
};
export type Overview = { name: string; root: string; repo: string; startedAt: number };
export const overview = source<Overview>("nts.overview");
export const issues = source<Issue[]>("nts.issues");
export const agents = source<Agent[]>("nts.agents");
export const plan = source<string>("nts.plan");
export const terminalPrograms = source<{ id: string; label: string }[]>("nts.terminal-programs");
export const startTerminal = operation<{ program: string }, { id: string }>("nts.start-terminal");
export const tabs = [
  "Now",
  "Tasks",
  "Agents",
  "Landing",
  "Full check",
  "Benchmarks",
  "Conformance",
  "Plan",
  "Machine",
  "Health",
  "Needs you",
] as const;
export type Tab = (typeof tabs)[number];
export const tabKey = (index: number) => (index < 9 ? String(index + 1) : index === 9 ? "0" : "");
