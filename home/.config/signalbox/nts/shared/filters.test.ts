import assert from "node:assert/strict";
import { test } from "node:test";
import { chosenOf, filterOptions, keysOf, matches, toggleFilter } from "./filters.ts";
import type { Chosen, Facet } from "./filters.ts";

test("task facets combine values, count against other facets, and retain selected values which disappear", () => {
  const items = [
    { id: 1, area: "compiler", labels: ["foundation", "foundation"] },
    { id: 2, area: "runtime", labels: ["foundation"] },
    { id: 3, area: "compiler", labels: ["bug"] },
    { id: 4, area: "docs", labels: [] },
  ];
  const facets: Facet<(typeof items)[number]>[] = [
    { id: "area", label: "area", values: (item) => [item.area] },
    { id: "label", label: "label", values: (item) => item.labels },
  ];
  let chosen: Chosen = toggleFilter(new Map(), "area", "compiler");
  const kept = () => items.filter((item) => matches(facets, chosen, item)).map((item) => item.id);
  assert.deepEqual(kept(), [1, 3]);
  assert.equal(
    filterOptions(facets, chosen, items).find((option) => option.value === "foundation")?.count,
    1,
  );
  chosen = toggleFilter(chosen, "area", "runtime");
  assert.deepEqual(kept(), [1, 2, 3]);
  chosen = toggleFilter(chosen, "label", "foundation");
  assert.deepEqual(kept(), [1, 2]);
  chosen = toggleFilter(chosen, "area", "compiler");
  assert.deepEqual(kept(), [2]);
  const remaining = filterOptions(facets, chosen, items.slice(2));
  assert.ok(
    remaining.some((option) => option.value === "runtime" && option.selected && option.count === 0),
  );
  assert.deepEqual(
    items.filter((item) => matches(facets, new Map(), item)),
    items,
  );
});

test("a choice travels as facet:value keys, a value's own colons kept", () => {
  const chosen = chosenOf(["area:tooling", "label:area:compiler", "area:compiler", "nonsense"]);
  assert.deepEqual([...chosen.get("area")!], ["tooling", "compiler"]);
  assert.deepEqual([...chosen.get("label")!], ["area:compiler"]);
  assert.equal(chosen.size, 2, "a key without a facet is dropped");
  assert.deepEqual(keysOf(chosen), ["area:tooling", "area:compiler", "label:area:compiler"]);
});
