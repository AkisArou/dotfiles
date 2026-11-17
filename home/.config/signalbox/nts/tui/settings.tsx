/** @jsxImportSource @opentui/react */
// The Settings rows at the end of the Agents tab: settings.json itself, worker
// slots, rounds before escalation, the pause, each role's models and how one is
// picked, and each harness's availability. Enter on a row offers its changes in
// the finder (or opens the file in the editor); the host writes settings.json,
// the only source of these values, as it does for the web view and sbn.
import { basename, dirname } from "node:path";
import type { Project } from "../shared/use-project.ts";
import type { AgentRole, Harness, Pick, SettingsView } from "../shared/model.ts";
import { modelsLine, pickLabels, sharePercents } from "../shared/model.ts";
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
/** What Enter does on a row: one choice in the finder, several, or a program on the terminal. */
export type SettingAction =
  | {
      readonly kind: "one";
      readonly lines: readonly string[];
      apply(index: number): Promise<unknown>;
    }
  | {
      readonly kind: "many";
      readonly lines: readonly string[];
      /** The lines chosen now, in their order; the finder starts with them ticked. */
      readonly chosen: readonly number[];
      apply(indices: readonly number[]): Promise<unknown>;
    }
  | { readonly kind: "run"; readonly run: TerminalCommand; readonly cwd: string };

const ROLES: Record<AgentRole, string> = {
  coordinator: "Coordinator",
  worker: "Worker",
  reviewer: "Reviewer",
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
  // Right after Worker slots: how those slots are shared between groups of areas.
  rows.splice(
    2,
    0,
    ...(settings.shares ?? []).map((group) => ({
      id: `setting:share:${group.name}`,
      label: `${group.name[0]!.toUpperCase()}${group.name.slice(1)} share`,
      value: `${group.percent}% · ${group.running} running`,
      warn: false,
      detail: [
        `${group.percent}% of the worker slots, for tasks in ${group.areas.join(", ")}.`,
        "",
        "A freed slot goes to the waiting task whose group is furthest below its share.",
        "A group with nothing waiting lends its share, so no worker idles while a task",
        "waits; nothing running is stopped. Enter sets this share; the others take the",
        "rest. The groups' areas are in settings.json.",
      ],
    })),
  );
  for (const role of Object.keys(ROLES) as AgentRole[]) {
    const { pick, models } = settings.roles[role];
    rows.push({
      id: `setting:models:${role}`,
      label: `${ROLES[role]} models`,
      value: modelsLine(settings.roles[role]),
      warn: false,
      detail: [
        ...models.map((model, index) => `${index + 1}. ${model.label}   ${model.id}`),
        "",
        "Enter offers every model at every effort, these ticked. Tab ticks or unticks;",
        "the order ticked is the order tried. The next launch of this role uses one of",
        "these; running agents keep theirs, and an unavailable harness is skipped.",
      ],
    });
    // The coordinator runs one at a time: it takes the first available, with nothing to choose.
    if (PICKS[role].length > 1)
      rows.push({
        id: `setting:pick:${role}`,
        label: `${ROLES[role]} pick`,
        value: pick,
        warn: false,
        detail: [
          `Picks ${pickLabels[pick]}.`,
          "",
          ...PICKS[role].map((way) => `${way}: ${pickLabels[way]}`),
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
      kind: "run",
      run: { ...editor, args: [...(editor.args ?? []), settings.path] },
      cwd: dirname(settings.path),
    };
  const share = /^setting:share:(.+)$/.exec(row.id)?.[1];
  const group = settings.shares?.find((each) => each.name === share);
  if (group) {
    const percents = sharePercents(group.percent);
    return {
      kind: "one",
      lines: percents.map(
        (percent) => `${mark(percent === group.percent)} ${group.name} ${percent}%`,
      ),
      apply: (index) =>
        commands.setSettings.run({
          share: { name: group.name, percent: percents[index]! },
          version,
        }),
    };
  }
  if (row.id === "setting:workers" || row.id === "setting:rounds") {
    const workers = row.id === "setting:workers";
    const values = workers ? range(0, 8) : range(1, 10);
    const now = workers ? settings.workerSlots : settings.maxRounds;
    return {
      kind: "one",
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
      ? { kind: "one", lines: ["Resume new tasks"], apply: () => commands.resumeTasks.run({}) }
      : {
          kind: "one",
          lines: PAUSE_REASONS.map((reason) => `Pause: ${reason}`),
          apply: (index) => commands.pauseTasks.run({ reason: PAUSE_REASONS[index]! }),
        };
  const modelsOf = /^setting:models:(\w+)$/.exec(row.id)?.[1] as AgentRole | undefined;
  if (modelsOf) {
    const ids = settings.catalog.map((model) => model.id);
    return {
      kind: "many",
      lines: settings.catalog.map((model) => `${model.label}   ${model.id}`),
      chosen: settings.roles[modelsOf].models.map((model) => ids.indexOf(model.id)),
      apply: (indices) =>
        commands.setSettings.run({
          roles: { [modelsOf]: { models: indices.map((index) => ids[index]!) } },
          version,
        }),
    };
  }
  const pickOf = /^setting:pick:(\w+)$/.exec(row.id)?.[1] as AgentRole | undefined;
  if (pickOf) {
    const ways = PICKS[pickOf];
    return {
      kind: "one",
      lines: ways.map(
        (way) => `${mark(way === settings.roles[pickOf].pick)} ${way}: ${pickLabels[way]}`,
      ),
      apply: (index) =>
        commands.setSettings.run({ roles: { [pickOf]: { pick: ways[index]! } }, version }),
    };
  }
  const harness = /^setting:harness:(\w+)$/.exec(row.id)?.[1] as Harness | undefined;
  if (harness) {
    const states = [null, ...UNAVAILABLE.map((reason) => ({ reason }))];
    const blocked = settings.unavailable[harness]?.reason;
    return {
      kind: "one",
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
