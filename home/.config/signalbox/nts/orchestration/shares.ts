// How the worker slots are shared between groups of work: by stream (its parent
// issue, such as #468 Foundations) or by area. workerSlots stays
// the one count of tasks holding a worker; a share only decides who gets the
// next free slot: the waiting task whose group is furthest below its share of
// the slots in use. A group with nothing waiting lends its share (no worker
// idles while a task waits), and nothing running is ever stopped. Within a
// group, urgency and then age decide, as without shares.
import type { Assignment } from "signalbox/ext/assignments";

export interface Share {
  readonly name: string;
  /** A relative weight: shares 1 and 1, or 50 and 50, both split evenly. 0 takes a slot only when no other group waits. */
  readonly share: number;
  /** The issues' area labels (area:<name>) this group's tasks come from. */
  readonly areas: readonly string[];
  /** Streams (their parent issues' numbers) whose tasks this group takes, whatever their area. */
  readonly streams?: readonly number[];
}

/** Where a task's issue sits: its area label and its stream's parent issue. */
export type Place = {
  readonly area?: string | undefined;
  readonly stream?: number | null | undefined;
};

/** Areas the shares must cover: every area the repository labels, so no task falls outside them. */
export function uncoveredAreas(shares: readonly Share[], known: readonly string[]): string[] {
  const named = new Set(shares.flatMap((group) => group.areas));
  return known.filter((area) => !named.has(area));
}

/** A task's group: the one naming its stream, else the one naming its area. */
export function groupOf(shares: readonly Share[], place: Place | undefined): Share | undefined {
  if (!place) return undefined;
  const { area, stream } = place;
  return (
    (stream ? shares.find((group) => group.streams?.includes(stream)) : undefined) ??
    (area === undefined ? undefined : shares.find((group) => group.areas.includes(area)))
  );
}

const holdsWorker = (assignment: Assignment) =>
  assignment.heldResources.some((held) => held.resource === "worker");

/**
 * The service's order for a waiting task (lower first): its group's load, the
 * tasks it already runs against its share, then urgency (-9..9 as ranks). One
 * more running task in a group always outweighs any urgency, so shares hold;
 * a task outside every group (an issue with no area label) ranks as if its
 * group ran nothing.
 */
export function shareRank(options: {
  readonly shares: readonly Share[] | undefined;
  readonly assignment: Assignment;
  readonly all: readonly Assignment[];
  readonly placeOf: (assignment: Assignment) => Place | undefined;
  readonly urgency: number;
}): number {
  const { shares, assignment, all, placeOf, urgency } = options;
  if (!shares?.length) return urgency;
  const group = groupOf(shares, placeOf(assignment));
  if (!group) return urgency;
  const total = shares.reduce((sum, each) => sum + each.share, 0);
  // A share of nothing waits behind every other group.
  if (group.share === 0) return 1e6 + urgency;
  const running = all.filter(
    (other) => holdsWorker(other) && groupOf(shares, placeOf(other)) === group,
  ).length;
  return 20 * running * (total / group.share) + urgency;
}

/** Sets one group to a percentage of the whole and scales the others to the rest, in whole percents. */
export function withPercent(shares: readonly Share[], name: string, percent: number): Share[] {
  const target = shares.find((group) => group.name === name);
  if (!target)
    throw new Error(`No share named ${name}: ${shares.map((group) => group.name).join(", ")}`);
  if (!Number.isInteger(percent) || percent < 0 || percent > 100)
    throw new Error("A share is a whole percent from 0 to 100");
  const others = shares.filter((group) => group !== target);
  const rest = others.reduce((sum, group) => sum + group.share, 0);
  let left = 100 - percent;
  return shares.map((group) => {
    if (group === target) return { ...group, share: percent };
    // The others keep their proportions; the last one takes what rounding left.
    const last = group === others.at(-1);
    const share = last
      ? left
      : Math.round((100 - percent) * (rest ? group.share / rest : 1 / others.length));
    left -= share;
    return { ...group, share };
  });
}

/** Each group's share as a whole percent of the total. */
export function percents(shares: readonly Share[]): Map<string, number> {
  const total = shares.reduce((sum, group) => sum + group.share, 0);
  return new Map(
    shares.map((group) => [group.name, total ? Math.round((group.share / total) * 100) : 0]),
  );
}
