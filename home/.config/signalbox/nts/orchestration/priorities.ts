// How urgent each task is. The assignment service gives freed worker slots and
// files to waiting tasks in this order (most urgent first, then the oldest), so
// the coordinator or the user can move a task ahead without cancelling others.
// Above urgency is the owner's next-slot list: tasks that take the next free
// worker slots, in order, whatever the shares (a task that unblocks others).
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { WorkflowStore } from "signalbox/ext/assignments";

const NAMESPACE = "nts/priorities";
const KEY = "urgency";
const NEXT = "next";
const nextSchema = z.array(z.number().int().positive());
/** One task's urgency: 0 is the default, higher goes first. */
export const urgencyValue = z.number().int().min(-9).max(9);
/** Issue number (as a string) to urgency; unset is 0. */
const urgencySchema = z.record(z.string(), urgencyValue);
export type Urgency = Readonly<Record<string, number>>;

export interface Priorities {
  readonly urgency: Readable<Urgency>;
  /** Issues that take the next free worker slots, in order, ahead of the shares. */
  readonly next: Readable<readonly number[]>;
  /** Sets an issue's urgency (0 clears it). */
  set(issue: number, urgency: number): Promise<Urgency>;
  /** Puts an issue at the end of the next-slot list, or takes it off. */
  setNext(issue: number, on: boolean): Promise<readonly number[]>;
}

export function priorities(options: { readonly state: Capability<WorkflowStore> }): {
  readonly extension: Extension;
  readonly service: Capability<Priorities>;
  /** The assignment service's order for a task's issue: lower runs first. */
  readonly rank: (issue: number | undefined) => number;
  /** Its place ahead of every share when it is on the next-slot list, else undefined. */
  readonly jump: (issue: number | undefined) => number | undefined;
} {
  const service = capability<Priorities>(NAMESPACE);
  const cell = new Cell<Urgency>({});
  const nextCell = new Cell<readonly number[]>([]);
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
    async setNext(issue, on) {
      const next = await store.transaction(NAMESPACE, (tx) => {
        const current = nextSchema.parse(tx.get(NEXT) ?? []);
        const updated = on
          ? current.includes(issue)
            ? current
            : [...current, issue]
          : current.filter((each) => each !== issue);
        tx.set(NEXT, updated);
        return updated;
      });
      nextCell.set(next);
      return next;
    },
    next: nextCell,
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
    jump: (issue) => {
      const reading = nextCell.get();
      const at =
        reading.status === "ready" && issue !== undefined ? reading.value.indexOf(issue) : -1;
      return at < 0 ? undefined : -1e9 + at;
    },
    extension: {
      id: NAMESPACE,
      requires: [options.state],
      provides: [provide(service, instance)],
      async start(context) {
        store = context.get(options.state);
        const [urgency, next] = await store.transaction(
          NAMESPACE,
          (tx) =>
            [urgencySchema.parse(tx.get(KEY) ?? {}), nextSchema.parse(tx.get(NEXT) ?? [])] as const,
        );
        cell.set(urgency);
        nextCell.set(next);
      },
    },
  };
}
