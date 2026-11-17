// The Settings rows for worker shares: one row per group after Worker slots,
// whose Enter offers whole tens of percent and sends that group's new share.
import { test } from "node:test";
import assert from "node:assert/strict";
import type { Project } from "../shared/use-project.ts";
import type { SettingsView } from "../shared/model.ts";
import { settingAction, settingRows } from "./settings.tsx";

const settings = {
  path: "/apps/nts/settings.json",
  version: "v1",
  error: null,
  workerSlots: 2,
  maxRounds: 4,
  shares: [
    { name: "compiler", percent: 50, areas: ["compiler", "runtime"], running: 1 },
    { name: "tooling", percent: 50, areas: ["tooling", "docs"], running: 1 },
  ],
  roles: {
    coordinator: { pick: "first", models: [] },
    worker: { pick: "coordinator", models: [] },
    reviewer: { pick: "first", models: [] },
  },
  paused: null,
  unavailable: {},
  catalog: [],
} as unknown as SettingsView;

test("each worker share is a Settings row after Worker slots; Enter sets its percent", async () => {
  const rows = settingRows(settings);
  assert.deepEqual(
    rows.slice(1, 4).map((row) => [row.label, row.value]),
    [
      ["Worker slots", "2"],
      ["Compiler share", "50% · 1 running"],
      ["Tooling share", "50% · 1 running"],
    ],
  );
  const sent: unknown[] = [];
  const project = {
    data: { settings },
    commands: { setSettings: { run: async (input: unknown) => void sent.push(input) } },
  } as unknown as Project;
  const action = settingAction(rows[2]!, project, { command: "true" });
  assert.equal(action?.kind, "one");
  if (action?.kind !== "one") return;
  assert.equal(action.lines.length, 11);
  assert.ok(action.lines.includes("✓ compiler 50%"), "the current share is marked");
  await action.apply(action.lines.indexOf("· compiler 70%"));
  assert.deepEqual(sent, [{ share: { name: "compiler", percent: 70 }, version: "v1" }]);
});
