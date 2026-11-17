// How the project runs right now: how many tasks hold a worker at once, and how
// many returns to build a task gets before it escalates. Like the profiles, these
// are live data in the host's state: a change applies without a restart, and the
// configuration only seeds a fresh state. Pausing new tasks is the assignment
// service's own durable pause, not a setting here.
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { Conflict } from "signalbox/core/utils";
import type { WorkflowStore } from "signalbox/ext/assignments";

const NAMESPACE = "nts/operations";
const KEY = "settings";
export const operationValues = z.object({
  /** Tasks holding a worker at once. 0 starts no build; tasks past their build still finish. */
  workerSlots: z.number().int().min(0).max(16),
  /** Returns to build (review findings, landing refusals) before a task escalates. */
  maxRounds: z.number().int().min(1).max(20),
});
export const operationSettings = operationValues.extend({
  revision: z.number().int().nonnegative(),
});
export type OperationValues = z.infer<typeof operationValues>;
export type OperationSettings = z.infer<typeof operationSettings>;

export interface Operations {
  readonly settings: Readable<OperationSettings>;
  /** Changes some values; with expectedRevision, only if nobody changed them since. */
  set(change: Partial<OperationValues>, expectedRevision?: number): Promise<OperationSettings>;
}

export function operations(options: {
  readonly state: Capability<WorkflowStore>;
  /** The values a fresh state starts from; later changes live in the state. */
  readonly initial: OperationValues;
}): {
  readonly extension: Extension;
  readonly service: Capability<Operations>;
  /** The values in force. Before the store has loaded, the initial ones. */
  readonly current: () => OperationSettings;
} {
  const service = capability<Operations>(NAMESPACE);
  const fresh: OperationSettings = { revision: 0, ...operationValues.parse(options.initial) };
  const cell = new Cell<OperationSettings>();
  let store: WorkflowStore;
  const read = (stored: unknown) =>
    stored === undefined ? fresh : operationSettings.parse(stored);
  const instance: Operations = {
    settings: cell,
    async set(change, expectedRevision) {
      const next = await store.transaction(NAMESPACE, (tx) => {
        const now = read(tx.get(KEY));
        if (expectedRevision !== undefined && expectedRevision !== now.revision)
          throw new Conflict(
            `Operation settings changed (revision ${now.revision}, expected ${expectedRevision})`,
          );
        const updated = operationSettings.parse({
          ...now,
          ...operationValues.partial().parse(change),
          revision: now.revision + 1,
        });
        tx.set(KEY, updated as unknown as Json);
        return updated;
      });
      cell.set(next);
      return next;
    },
  };
  return {
    service,
    current: () => {
      const reading = cell.get();
      return reading.status === "ready" ? reading.value : fresh;
    },
    extension: {
      id: NAMESPACE,
      requires: [options.state],
      provides: [provide(service, instance)],
      async start(context) {
        store = context.get(options.state);
        cell.set(await store.transaction(NAMESPACE, (tx) => read(tx.get(KEY))));
      },
    },
  };
}
