import assert from "node:assert/strict";
import { test } from "node:test";
import {
  chosenOf,
  describeTaskFilter,
  filterOptions,
  keysOf,
  matches,
  taskFacets,
  toggleFilter,
} from "./filters.ts";
import type { Issue } from "./model.ts";
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

test("each filter value reads and means what the glossary and the project's labels say", () => {
  const issue = {
    area: "tooling",
    stream: "Tooling",
    labels: ["task", "kind:foundation", "area:tooling"],
    state: "in_progress",
    owner: null,
  } as unknown as Issue;
  const options = filterOptions(
    taskFacets,
    new Map(),
    [issue],
    describeTaskFilter({
      task: "An agent can take it",
      "kind:foundation": "A foundation or the tooling it stands on",
      "area:tooling": "Tooling and the gate",
    }),
  );
  const find = (label: string) => options.find((option) => option.label === label);
  assert.equal(find("state: in progress")?.meaning?.startsWith("being worked on"), true);
  assert.equal(find("kind: foundation")?.meaning, "A foundation or the tooling it stands on");
  assert.equal(find("area: tooling")?.meaning, "Tooling and the gate");
  assert.equal(find("label: task")?.meaning, "An agent can take it");
  assert.equal(find("assigned: no")?.meaning, "nobody owns it");
  assert.equal(find("stream: Tooling")?.meaning, undefined);
  assert.equal(find("state: in progress")?.value, "in_progress", "the key keeps the value");
});
