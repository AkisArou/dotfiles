/** @jsxImportSource @opentui/react */
// The settings rows at the top of the Agents tab: how many tasks hold a worker,
// the rounds before escalation, the pause, which profile each role launches with,
// and which harnesses are available. Enter on a row offers its values in the
// finder; the change goes to the host, as from the web view or sbn.
import type { Project } from "../shared/use-project.ts";
import type { AgentRole, Harness } from "../shared/model.ts";
import { Line, Section } from "./components.tsx";
import { colors as c } from "./theme.ts";

export interface SettingRow {
  readonly id: string;
  readonly label: string;
  readonly value: string;
  readonly warn: boolean;
  readonly describe: string;
}
export interface SettingChoice {
  readonly lines: readonly string[];
  apply(index: number): Promise<unknown>;
}

// A role row's value is the profile it launches with.
const ROLES: Record<AgentRole, string> = {
  coordinator: "Coordinator",
  worker: "Workers",
  reviewer: "Reviewers",
};
const HARNESSES: Record<Harness, string> = { claude: "Claude Code", codex: "Codex" };
const PAUSE_REASONS = [
  "the owner paused new tasks",
  "usage is running low",
  "the machine is busy",
  "recovering after a power cut",
];
const UNAVAILABLE = ["usage exhausted", "login expired"];
const range = (from: number, to: number) =>
  Array.from({ length: to - from + 1 }, (_, index) => from + index);
const mark = (current: boolean) => (current ? "✓" : "·");

export function settingRows(project: Project): SettingRow[] {
  const { operations, profiles } = project.data;
  const rows: SettingRow[] = [];
  if (operations)
    rows.push(
      {
        id: "setting:workers",
        label: "Worker slots",
        value: String(operations.workerSlots),
        warn: operations.workerSlots === 0,
        describe:
          "Tasks holding a worker at once. Raising it starts waiting tasks now; lowering it never stops one, it only holds back the next.",
      },
      {
        id: "setting:rounds",
        label: "Rounds before escalation",
        value: String(operations.maxRounds),
        warn: false,
        describe:
          "How many times a task goes back to build (review findings, landing refusals) before it escalates to the coordinator.",
      },
      {
        id: "setting:pause",
        label: "New tasks",
        value: operations.paused ? `PAUSED: ${operations.paused.reason}` : "admitted",
        warn: !!operations.paused,
        describe:
          "Paused, queued tasks wait and none is admitted; admitted tasks carry on to the end. The coordinator sees the reason.",
      },
    );
  if (profiles) {
    const label = (id: string) => profiles.profiles.find((p) => p.id === id)?.label ?? id;
    for (const role of Object.keys(ROLES) as AgentRole[]) {
      const choice = profiles.roles[role];
      rows.push({
        id: `setting:role:${role}`,
        label: ROLES[role],
        value: `${label(choice.profile)}${choice.fallback ? `, else ${label(choice.fallback)}` : ""}`,
        warn: false,
        describe:
          "The harness, model and effort the next launch of this role uses; running agents keep theirs. A fallback is used while the profile's harness is unavailable.",
      });
    }
    for (const harness of Object.keys(HARNESSES) as Harness[]) {
      const blocked = profiles.unavailable[harness];
      rows.push({
        id: `setting:harness:${harness}`,
        label: HARNESSES[harness],
        value: blocked ? `unavailable: ${blocked.reason}` : "available",
        warn: !!blocked,
        describe:
          "An unavailable harness launches nothing: roles on it wait, or use their fallback. The host marks Claude Code unavailable itself when it stops answering.",
      });
    }
  }
  return rows;
}

/** The values Enter offers for a row, and the change each one makes. */
export function settingChoice(row: SettingRow, project: Project): SettingChoice | undefined {
  const { operations, profiles } = project.data;
  const { commands } = project;
  if (operations && (row.id === "setting:workers" || row.id === "setting:rounds")) {
    const workers = row.id === "setting:workers";
    const values = workers ? range(0, 8) : range(1, 10);
    const now = workers ? operations.workerSlots : operations.maxRounds;
    return {
      lines: values.map((n) => `${mark(n === now)} ${n} ${workers ? "worker slots" : "rounds"}`),
      apply: (index) =>
        commands.setOperations.run({
          [workers ? "workerSlots" : "maxRounds"]: values[index]!,
          revision: operations.revision,
        }),
    };
  }
  if (operations && row.id === "setting:pause")
    return operations.paused
      ? { lines: ["Resume new tasks"], apply: () => commands.resumeTasks.run({}) }
      : {
          lines: PAUSE_REASONS.map((reason) => `Pause: ${reason}`),
          apply: (index) => commands.pauseTasks.run({ reason: PAUSE_REASONS[index]! }),
        };
  const role = /^setting:role:(\w+)$/.exec(row.id)?.[1] as AgentRole | undefined;
  if (profiles && role) {
    const current = profiles.roles[role];
    const options = [
      ...profiles.profiles.map((p) => ({
        line: `${mark(p.id === current.profile)} use ${p.label}`,
        next: { profile: p.id, fallback: current.fallback === p.id ? undefined : current.fallback },
      })),
      {
        line: `${mark(!current.fallback)} no fallback (wait while unavailable)`,
        next: { profile: current.profile, fallback: undefined },
      },
      ...profiles.profiles
        .filter((p) => p.id !== current.profile)
        .map((p) => ({
          line: `${mark(p.id === current.fallback)} fallback ${p.label}`,
          next: { profile: current.profile, fallback: p.id },
        })),
    ];
    return {
      lines: options.map((option) => option.line),
      apply: (index) => {
        const { profile, fallback } = options[index]!.next;
        return commands.setRole.run({
          role,
          profile,
          ...(fallback ? { fallback } : {}),
          revision: profiles.revision,
        });
      },
    };
  }
  const harness = /^setting:harness:(\w+)$/.exec(row.id)?.[1] as Harness | undefined;
  if (profiles && harness) {
    const states = [null, ...UNAVAILABLE.map((reason) => ({ reason }))];
    const blocked = profiles.unavailable[harness]?.reason;
    return {
      lines: states.map((state) =>
        state
          ? `${mark(blocked === state.reason)} unavailable: ${state.reason}`
          : `${mark(!blocked)} available`,
      ),
      apply: (index) => commands.setAvailability.run({ harness, unavailable: states[index]! }),
    };
  }
  return undefined;
}

export function SettingDetail({ row }: { row: SettingRow }) {
  return (
    <Section title={row.label}>
      <Line color={row.warn ? c.warning : c.text}>{row.value}</Line>
      <Line color={c.muted}>{row.describe}</Line>
      <Line color={c.muted}>Enter changes it; the host applies it without a restart.</Line>
    </Section>
  );
}
