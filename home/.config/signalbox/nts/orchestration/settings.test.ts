// settings.json is the only source of the agent settings: the host follows hand
// edits as they are saved, refuses an invalid one while the last good settings
// stay in force, writes clients' changes to the file, and refuses a change made
// from a stale version or while the file has an error.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { defineConfig, latestValue, startExtensions } from "signalbox/core";
import { settingsFile } from "./settings.ts";
import type { SettingsReading } from "./settings.ts";
import { catalog, profileFromId, profileId, profileLabel } from "./models.ts";
import { chooseProfile } from "./profiles.ts";

const opus = { harness: "claude", model: "claude-opus-5-5", effort: "high" } as const;
const codex = { harness: "codex", effort: "high" } as const;
const valid = (workerSlots = 2) => ({
  workerSlots,
  maxRounds: 4,
  roles: {
    coordinator: { pick: "first", models: [opus] },
    worker: { pick: "coordinator", models: [opus, codex] },
    reviewer: { pick: "first", models: [opus] },
  },
});

async function until(
  read: () => SettingsReading | undefined,
  done: (r: SettingsReading) => boolean,
) {
  const deadline = Date.now() + 5_000;
  for (;;) {
    const reading = read();
    if (reading && done(reading)) return reading;
    if (Date.now() > deadline) throw new Error(`still ${JSON.stringify(reading)}`);
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
}

test("settings.json is followed live, written by clients, and an invalid edit keeps the last good settings", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-settings-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const path = join(directory, "settings.json");
  await writeFile(path, JSON.stringify(valid()));
  const file = settingsFile({ path });
  const app = await startExtensions(
    defineConfig({
      id: "settings",
      root: directory,
      gate: { accept: () => false },
      extensions: [file.extension],
    }),
  );
  t.after(() => app.stop());
  const service = app.context.get(file.service);
  const reading = () => latestValue(service.reading);
  assert.equal(file.current().workerSlots, 2);

  // A hand edit applies without a restart.
  await writeFile(path, JSON.stringify(valid(3)));
  const edited = await until(reading, (r) => r.settings.workerSlots === 3);
  assert.equal(edited.error, null);

  // An invalid hand edit is refused by name; the last good settings stay in force.
  await writeFile(path, JSON.stringify({ ...valid(5), workerSlot: 1 }));
  const refused = await until(reading, (r) => r.error !== null);
  assert.match(refused.error!, /workerSlot/);
  assert.equal(file.current().workerSlots, 3, "the last good settings stay");
  await assert.rejects(
    service.update((s) => ({ ...s, maxRounds: 5 })),
    /fix it first/,
  );

  // Fixed by hand, a client's change is written to the file.
  await writeFile(path, JSON.stringify(valid(3)));
  const fixed = await until(reading, (r) => r.error === null);
  const written = await service.update((s) => ({ ...s, maxRounds: 6 }), fixed.version);
  assert.equal(JSON.parse(await readFile(path, "utf8")).maxRounds, 6);
  assert.notEqual(written.version, fixed.version);
  await assert.rejects(
    service.update((s) => ({ ...s, maxRounds: 7 }), fixed.version),
    /changed since it was read/,
    "a change from a stale version is refused",
  );
  await assert.rejects(
    service.update((s) => ({
      ...s,
      roles: { ...s.roles, coordinator: { ...s.roles.coordinator, pick: "spread" } },
    })),
    /must be first/,
  );
  await assert.rejects(
    service.update((s) => ({ ...s, roles: { ...s.roles, worker: { pick: "first", models: [] } } })),
    /at least 1|too small/i,
  );
});

test("a host refuses to start on a missing or invalid settings.json, naming it", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-settings-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const path = join(directory, "settings.json");
  const start = () =>
    startExtensions(
      defineConfig({
        id: "settings",
        root: directory,
        gate: { accept: () => false },
        extensions: [settingsFile({ path }).extension],
      }),
    );
  await assert.rejects(start(), /settings\.json/);
  await writeFile(path, "{ not json");
  await assert.rejects(start(), /not JSON/);
});

test("a role's models are chosen first, spread or by the coordinator, skipping unavailable harnesses", () => {
  const models = [opus, codex];
  const id = (r: ReturnType<typeof chooseProfile>) =>
    "profile" in r ? profileId(r.profile) : r.wait;
  const now = 1_000;
  assert.equal(
    id(chooseProfile("worker", { pick: "first", models }, {})),
    "claude/claude-opus-5-5/high",
  );
  assert.equal(
    id(
      chooseProfile("worker", { pick: "first", models }, { claude: { reason: "usage exhausted" } }),
    ),
    "codex/configured/high",
    "an unavailable harness is skipped",
  );
  const inUse = new Map([
    ["claude/claude-opus-5-5/high", 2],
    ["codex/configured/high", 1],
  ]);
  assert.equal(
    id(chooseProfile("worker", { pick: "spread", models }, {}, { inUse })),
    "codex/configured/high",
  );
  assert.equal(
    id(chooseProfile("worker", { pick: "spread", models }, {}, { inUse: new Map() })),
    "claude/claude-opus-5-5/high",
    "ties keep the order",
  );
  assert.equal(
    id(
      chooseProfile(
        "worker",
        { pick: "coordinator", models },
        {},
        { preferred: "codex/configured/high" },
      ),
    ),
    "codex/configured/high",
  );
  assert.equal(
    id(
      chooseProfile(
        "worker",
        { pick: "first", models },
        {},
        { preferred: "codex/configured/high" },
      ),
    ),
    "claude/claude-opus-5-5/high",
    "a named model counts only when the role picks coordinator",
  );
  assert.equal(
    id(
      chooseProfile(
        "worker",
        { pick: "coordinator", models },
        { codex: { reason: "login expired" } },
        {
          preferred: "codex/configured/high",
        },
      ),
    ),
    "claude/claude-opus-5-5/high",
    "a named model whose harness is down falls back to the first available",
  );
  const waiting = chooseProfile(
    "worker",
    { pick: "first", models },
    {
      claude: { reason: "usage exhausted", until: now + 60_000 },
      codex: { reason: "login expired" },
    },
    { now },
  );
  assert.ok("wait" in waiting);
  assert.match(waiting.wait, /Claude Code \(usage exhausted\), Codex \(login expired\)/);
  assert.equal(waiting.retryAt, now + 60_000, "it retries when the first harness comes back");
});

test("the catalog offers every model at every effort, by stable ids", () => {
  assert.equal(catalog.length, 4 * 5 + 5);
  for (const profile of catalog) assert.deepEqual(profileFromId(profileId(profile)), profile);
  assert.equal(profileLabel(opus), "Opus 5.5 · high");
  assert.deepEqual(profileFromId("codex/gpt-6.1-sol/xhigh"), {
    harness: "codex",
    model: "gpt-6.1-sol",
    effort: "xhigh",
  });
  assert.throws(() => profileFromId("claude/claude-opus-55/high"), /not a profile/);
  assert.throws(() => profileFromId("claude/claude-opus-5-5/minimal"), /not a profile/);
});
