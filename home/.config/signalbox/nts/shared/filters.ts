// Task filters, the same in the TUI and the web: facets of an issue, OR within a
// facet and AND between facets. Task states and workflow policy remain owned by
// the project.
import type { Issue } from "./model.ts";
export interface Facet<T> {
  readonly id: string;
  readonly label: string;
  readonly values: (item: T) => readonly string[];
}
export type Chosen = ReadonlyMap<string, ReadonlySet<string>>;
export type FilterOption = {
  facet: string;
  value: string;
  label: string;
  count: number;
  selected: boolean;
};

/** OR within a facet, AND between facets. */
export function matches<T>(facets: readonly Facet<T>[], chosen: Chosen, item: T): boolean {
  return facets.every((facet) => {
    const values = chosen.get(facet.id);
    return !values?.size || facet.values(item).some((value) => values.has(value));
  });
}

/** Counts include the other facets, so they describe the current search. */
export function filterOptions<T>(
  facets: readonly Facet<T>[],
  chosen: Chosen,
  items: readonly T[],
): FilterOption[] {
  return facets.flatMap((facet) => {
    const others = new Map([...chosen].filter(([id]) => id !== facet.id));
    const counts = new Map<string, number>();
    for (const item of items) {
      if (!matches(facets, others, item)) continue;
      for (const value of new Set(facet.values(item)))
        counts.set(value, (counts.get(value) ?? 0) + 1);
    }
    for (const value of chosen.get(facet.id) ?? []) if (!counts.has(value)) counts.set(value, 0);
    return [...counts]
      .sort(([a, ac], [b, bc]) => bc - ac || a.localeCompare(b))
      .map(([value, count]) => ({
        facet: facet.id,
        value,
        count,
        label: `${facet.label}: ${value}`,
        selected: chosen.get(facet.id)?.has(value) ?? false,
      }));
  });
}

export function toggleFilter(chosen: Chosen, facet: string, value: string): Chosen {
  const next = new Map(chosen),
    values = new Set(chosen.get(facet));
  if (!values.delete(value)) values.add(value);
  if (values.size) next.set(facet, values);
  else next.delete(facet);
  return next;
}

export function describeFilters<T>(facets: readonly Facet<T>[], chosen: Chosen): string {
  return facets
    .filter((facet) => chosen.get(facet.id)?.size)
    .map((facet) => `${facet.label}: ${[...chosen.get(facet.id)!].join(", ")}`)
    .join(" · ");
}

/** The facets tasks are filtered by. */
export const taskFacets = [
  { id: "area", label: "area", values: (issue) => [issue.area] },
  { id: "stream", label: "stream", values: (issue) => [issue.stream] },
  {
    id: "kind",
    label: "kind",
    values: (issue) =>
      issue.labels.filter((label) => label.startsWith("kind:")).map((label) => label.slice(5)),
  },
  { id: "state", label: "state", values: (issue) => [issue.state] },
  {
    id: "label",
    label: "label",
    values: (issue) => issue.labels.filter((label) => !label.startsWith("issue:")),
  },
  { id: "assigned", label: "assigned", values: (issue) => [issue.owner ? "yes" : "no"] },
] satisfies readonly Facet<Issue>[];

/** A chosen value as one string, facet:value, for a URL or a multi-select. */
export const filterKey = (facet: string, value: string) => `${facet}:${value}`;

/** The choice as keys, in facet order. */
export function keysOf(chosen: Chosen): string[] {
  return [...chosen].flatMap(([facet, values]) =>
    [...values].map((value) => filterKey(facet, value)),
  );
}

/** The choice that keys make; a value may hold colons (a label such as area:tooling). */
export function chosenOf(keys: readonly string[]): Chosen {
  const chosen = new Map<string, Set<string>>();
  for (const key of keys) {
    const at = key.indexOf(":");
    if (at <= 0) continue;
    const facet = key.slice(0, at);
    chosen.set(facet, (chosen.get(facet) ?? new Set()).add(key.slice(at + 1)));
  }
  return chosen;
}
