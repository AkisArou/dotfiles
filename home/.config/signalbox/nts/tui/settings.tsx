/** @jsxImportSource @opentui/react */
// The Settings rows at the end of the Agents tab: settings.json itself, worker
// slots, rounds before escalation, the pause, each role's models and how one is
// picked, and each harness's availability. Enter on a row offers its changes in
// the finder (or opens the file in the editor); the host writes settings.json,
// the only source of these values, as it does for the web view and sbn.
import { basename, dirname } from "node:path";
import type { Project } from "../shared/use-project.ts";
import type { AgentRole, Harness, Pick, SettingsView } from "../shared/model.ts";
import { modelsLine, pickLabels } from "../shared/model.ts";
import { Line, Section } from "./components.tsx";
import { colors as c } from "./theme.ts";
import type { TerminalCommand } from "./tools.ts";

export interface SettingRow {
  readonly id: string;
  readonly label: string;
  readonly value: string;
  readonly warn: boolean;
  readonly detail: readonly string[];
}
/** What Enter does on a row: offer choices in the finder, or run a program on the terminal. */
export type SettingAction =
  | { readonly lines: readonly string[]; apply(index: number): Promise<unknown> }
  | { readonly run: TerminalCommand; readonly cwd: string };

const ROLES: Record<AgentRole, string> = {
  coordinator: "Coordinator",
  worker: "Workers",
  reviewer: "Reviewers",
};
const HARNESSES: Record<Harness, string> = { claude: "Claude Code", codex: "Codex" };
/** The coordinator runs one at a time, so it only takes the first available. */
const PICKS: Record<AgentRole, readonly Pick[]> = {
  coordinator: ["first"],
  worker: ["first", "spread", "coordinator"],
  reviewer: ["first", "spread"],
};
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

export function settingRows(settings: SettingsView | undefined): SettingRow[] {
  if (!settings) return [];
  const rows: SettingRow[] = [
    {
      id: "setting:file",
      label: "Settings file",
      value: settings.error ? "ERROR: Enter edits it" : basename(settings.path),
      warn: !!settings.error,
      detail: [
        settings.path,
        ...(settings.error
          ? ["", "Not in force (the last good settings are):", settings.error]
          : []),
        "",
        "The only source of worker slots, rounds and each role's models. Enter opens it in $EDITOR;",
        "the host applies a saved edit at once, and refuses one that fails its checks.",
      ],
    },
    {
      id: "setting:workers",
      label: "Worker slots",
      value: String(settings.workerSlots),
      warn: settings.workerSlots === 0,
      detail: [
        "Tasks holding a worker at once. Raising it starts waiting tasks now;",
        "lowering it never stops one, it only holds back the next.",
      ],
    },
    {
      id: "setting:rounds",
      label: "Rounds before escalation",
      value: String(settings.maxRounds),
      warn: false,
      detail: [
        "How many times a task goes back to build (review findings, landing refusals)",
        "before it escalates to the coordinator.",
      ],
    },
    {
      id: "setting:pause",
      label: "New tasks",
      value: settings.paused ? `PAUSED: ${settings.paused.reason}` : "admitted",
      warn: !!settings.paused,
      detail: [
        "Paused, queued tasks wait and none is admitted; admitted tasks carry on to the end.",
        "The coordinator sees the reason. The pause is host state, not part of settings.json.",
      ],
    },
  ];
  for (const role of Object.keys(ROLES) as AgentRole[]) {
    const { pick, models } = settings.roles[role];
    rows.push({
      id: `setting:role:${role}`,
      label: ROLES[role],
      value: modelsLine(settings.roles[role]),
      warn: false,
      detail: [
        `Picks ${pickLabels[pick]}.`,
        ...models.map((model, index) => `${index + 1}. ${model.label}   ${model.id}`),
        "",
        "The next launch of this role uses one of these; running agents keep theirs.",
        "A model whose harness is unavailable is skipped.",
      ],
    });
  }
  for (const harness of Object.keys(HARNESSES) as Harness[]) {
    const blocked = settings.unavailable[harness];
    rows.push({
      id: `setting:harness:${harness}`,
      label: HARNESSES[harness],
      value: blocked ? `unavailable: ${blocked.reason}` : "available",
      warn: !!blocked,
      detail: [
        "An unavailable harness launches nothing; its models are skipped. The host marks",
        "Claude Code unavailable itself when it stops answering. Host state, not settings.json.",
      ],
    });
  }
  return rows;
}

/** What Enter on a row offers, and the change each choice makes. */
export function settingAction(
  row: SettingRow,
  project: Project,
  editor: TerminalCommand,
): SettingAction | undefined {
  const settings = project.data.settings;
  if (!settings) return undefined;
  const { commands } = project;
  const version = settings.version;
  if (row.id === "setting:file")
    return {
      run: { ...editor, args: [...(editor.args ?? []), settings.path] },
      cwd: dirname(settings.path),
    };
  if (row.id === "setting:workers" || row.id === "setting:rounds") {
    const workers = row.id === "setting:workers";
    const values = workers ? range(0, 8) : range(1, 10);
    const now = workers ? settings.workerSlots : settings.maxRounds;
    return {
      lines: values.map((n) => `${mark(n === now)} ${n} ${workers ? "worker slots" : "rounds"}`),
      apply: (index) =>
        commands.setSettings.run({
          [workers ? "workerSlots" : "maxRounds"]: values[index]!,
          version,
        }),
    };
  }
  if (row.id === "setting:pause")
    return settings.paused
      ? { lines: ["Resume new tasks"], apply: () => commands.resumeTasks.run({}) }
      : {
          lines: PAUSE_REASONS.map((reason) => `Pause: ${reason}`),
          apply: (index) => commands.pauseTasks.run({ reason: PAUSE_REASONS[index]! }),
        };
  const role = /^setting:role:(\w+)$/.exec(row.id)?.[1] as AgentRole | undefined;
  if (role) {
    const { pick, models } = settings.roles[role];
    const ids = models.map((model) => model.id);
    const listed = new Set(ids);
    const options: { line: string; change: { pick?: Pick; models?: string[] } }[] = [
      ...(PICKS[role].length > 1
        ? PICKS[role].map((way) => ({
            line: `${mark(way === pick)} pick: ${pickLabels[way]}`,
            change: { pick: way },
          }))
        : []),
      ...models.flatMap((model, index) => [
        ...(index > 0
          ? [
              {
                line: `↑ first: ${model.label}`,
                change: { models: [model.id, ...ids.filter((id) => id !== model.id)] },
              },
            ]
          : []),
        ...(models.length > 1
          ? [
              {
                line: `✕ remove: ${model.label}`,
                change: { models: ids.filter((id) => id !== model.id) },
              },
            ]
          : []),
      ]),
      ...settings.catalog
        .filter((model) => !listed.has(model.id))
        .map((model) => ({
          line: `+ add: ${model.label}`,
          change: { models: [...ids, model.id] },
        })),
    ];
    return {
      lines: options.map((option) => option.line),
      apply: (index) =>
        commands.setSettings.run({ roles: { [role]: options[index]!.change }, version }),
    };
  }
  const harness = /^setting:harness:(\w+)$/.exec(row.id)?.[1] as Harness | undefined;
  if (harness) {
    const states = [null, ...UNAVAILABLE.map((reason) => ({ reason }))];
    const blocked = settings.unavailable[harness]?.reason;
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
      {row.detail.map((line, index) => (
        <Line key={index} color={c.muted}>
          {line || " "}
        </Line>
      ))}
      <Line color={c.muted}> </Line>
      <Line color={c.muted}>Enter changes it.</Line>
    </Section>
  );
}
