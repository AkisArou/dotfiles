// View-local facets. Task states and workflow policy remain owned by the project.
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
