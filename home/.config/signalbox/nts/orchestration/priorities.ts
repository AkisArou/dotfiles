// How urgent each task is. The assignment service gives freed worker slots and
// files to waiting tasks in this order (most urgent first, then the oldest), so
// the coordinator or the user can move a task ahead without cancelling others.
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { WorkflowStore } from "signalbox/ext/assignments/index";

const NAMESPACE = "nts/priorities";
const KEY = "urgency";
/** One task's urgency: 0 is the default, higher goes first. */
export const urgencyValue = z.number().int().min(-9).max(9);
/** Issue number (as a string) to urgency; unset is 0. */
const urgencySchema = z.record(z.string(), urgencyValue);
export type Urgency = Readonly<Record<string, number>>;

export interface Priorities {
  readonly urgency: Readable<Urgency>;
  /** Sets an issue's urgency (0 clears it). */
  set(issue: number, urgency: number): Promise<Urgency>;
}

export function priorities(options: { readonly state: Capability<WorkflowStore> }): {
  readonly extension: Extension;
  readonly service: Capability<Priorities>;
  /** The assignment service's order for a task's issue: lower runs first. */
  readonly rank: (issue: number | undefined) => number;
} {
  const service = capability<Priorities>(NAMESPACE);
  const cell = new Cell<Urgency>({});
  let store: WorkflowStore;
  const instance: Priorities = {
    urgency: cell,
    async set(issue, urgency) {
      const next = await store.transaction(NAMESPACE, (tx) => {
        const current = urgencySchema.parse(tx.get(KEY) ?? {});
        const updated: Record<string, number> = {
          ...current,
          [String(issue)]: urgencyValue.parse(urgency),
        };
        if (urgency === 0) delete updated[String(issue)];
        tx.set(KEY, updated as Json);
        return updated;
      });
      cell.set(next);
      return next;
    },
  };
  return {
    service,
    rank: (issue) => {
      const reading = cell.get();
      return -(
        (reading.status === "ready" && issue !== undefined && reading.value[String(issue)]) ||
        0
      );
    },
    extension: {
      id: NAMESPACE,
      requires: [options.state],
      provides: [provide(service, instance)],
      async start(context) {
        store = context.get(options.state);
        cell.set(
          await store.transaction(NAMESPACE, (tx) => urgencySchema.parse(tx.get(KEY) ?? {})),
        );
      },
    },
  };
}
