// The operation settings are live state: a fresh state starts from the
// configuration, a change survives a restart and beats the configuration, and a
// stale revision or an out-of-range value is refused.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { defineConfig, latestValue, startExtensions } from "signalbox/core";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import { operations } from "./operations.ts";

async function host(directory: string, initial: { workerSlots: number; maxRounds: number }) {
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const running = operations({ state: state.state, initial });
  const app = await startExtensions(
    defineConfig({
      id: "operations",
      root: directory,
      gate: { accept: () => false },
      extensions: [state.extension, running.extension],
    }),
  );
  return { app, running, service: app.context.get(running.service) };
}

test("operation settings start from the configuration and then live in the state", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-operations-"));
  t.after(() => rm(directory, { recursive: true, force: true }));

  const first = await host(directory, { workerSlots: 2, maxRounds: 4 });
  assert.deepEqual(first.running.current(), { revision: 0, workerSlots: 2, maxRounds: 4 });
  const changed = await first.service.set({ workerSlots: 3 }, 0);
  assert.deepEqual(changed, { revision: 1, workerSlots: 3, maxRounds: 4 });
  assert.deepEqual(latestValue(first.service.settings), changed, "readers see the change at once");
  await assert.rejects(first.service.set({ maxRounds: 5 }, 0), /revision 1, expected 0/);
  await assert.rejects(first.service.set({ workerSlots: -1 }));
  await assert.rejects(first.service.set({ maxRounds: 0 }));
  assert.equal(first.running.current().revision, 1, "refused changes leave the settings alone");
  await first.app.stop();

  // A restart with different configured values keeps what was set.
  const second = await host(directory, { workerSlots: 1, maxRounds: 9 });
  assert.deepEqual(second.running.current(), { revision: 1, workerSlots: 3, maxRounds: 4 });
  await second.app.stop();
});
