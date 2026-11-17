import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { capability, Cell, defineConfig, provide, startExtensions } from "signalbox/core";
import type { Readable } from "signalbox/core";
import type { Assignment } from "signalbox/ext/assignments";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import { priorities } from "./priorities.ts";
import { nextLeavesOnStart } from "./index.ts";

test("the owner's next-slot list goes ahead of every share, in order, and a task leaves it once it starts", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-next-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const urgency = priorities({ state: state.state });
  const list = (service: { next: Readable<readonly number[]> }) => {
    const reading = service.next.get();
    return reading.status === "ready" ? reading.value : undefined;
  };
  const assignmentsKey = capability<Readable<readonly Assignment[]>>("assignments");
  const assignments = new Cell<readonly Assignment[]>([]);
  const assignment = (issue: number, worker: boolean) =>
    ({
      input: { issue },
      heldResources: worker ? [{ resource: "worker" }] : [],
    }) as unknown as Assignment;
  const start = () =>
    startExtensions(
      defineConfig({
        id: "next",
        root: directory,
        gate: { accept: () => false },
        extensions: [
          state.extension,
          urgency.extension,
          { id: "assignments", provides: [provide(assignmentsKey, assignments)] },
          nextLeavesOnStart(urgency.service, assignmentsKey),
        ],
      }),
    );
  let app = await start();
  const service = app.context.get(urgency.service);
  assert.equal(urgency.jump(842), undefined, "off the list, a task waits for its share");

  await service.setNext(842, true);
  await service.setNext(847, true);
  await service.setNext(842, true);
  assert.deepEqual(list(service), [842, 847], "once each, in order");
  assert.ok(urgency.jump(842)! < urgency.jump(847)!, "the first on the list goes first");
  assert.ok(urgency.jump(847)! < 1e6 * -1, "ahead of every share and urgency");

  // A restart keeps the list.
  await app.stop();
  app = await start();
  t.after(() => app.stop());
  const again = app.context.get(urgency.service);
  assert.deepEqual(list(again), [842, 847]);

  // Queued is not started; holding a worker is, and the task leaves the list.
  assignments.set([assignment(842, false)]);
  await new Promise((resolve) => setTimeout(resolve, 50));
  assert.deepEqual(list(again), [842, 847]);
  assignments.set([assignment(842, true)]);
  for (let i = 0; i < 40 && urgency.jump(842) !== undefined; i++)
    await new Promise((resolve) => setTimeout(resolve, 25));
  assert.equal(urgency.jump(842), undefined);
  assert.deepEqual(list(again), [847]);

  await again.setNext(847, false);
  assert.deepEqual(list(again), []);
});
