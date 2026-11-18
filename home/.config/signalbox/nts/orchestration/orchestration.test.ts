import { test } from "node:test";
import assert from "node:assert/strict";

test("each area's reading list names workflow sections and lessons that exist", async () => {
  const { readFile, access } = await import("node:fs/promises");
  const { readingList } = await import("./briefs.ts");
  const { ntsConfig } = await import("../config.ts");
  const workflow = await readFile(`${ntsConfig.root}/docs/workflow.md`, "utf8");
  const headings = new Set([...workflow.matchAll(/^## (.+)$/gm)].map((match) => match[1]!));
  for (const area of [
    "tooling",
    "compiler",
    "runtime",
    "jvm",
    "wasm",
    "node",
    "benchmarks",
    "chromium",
    "docs",
  ]) {
    const list = readingList([`area:${area}`]);
    for (const [, name] of list.matchAll(/"## ([^"]+)"/g))
      assert.ok(headings.has(name!), `${area}: no section ${name}`);
    const lessons = /^- docs\/contributing\/lessons\/: (.+)\.$/m.exec(list)?.[1] ?? "";
    for (const [, topic] of lessons.matchAll(/(\w+)\.md/g))
      await access(`${ntsConfig.root}/docs/contributing/lessons/${topic}.md`);
  }
  assert.match(readingList(["area:tooling"]), /concurrency\.md, fixtures\.md, machine\.md/);
  assert.doesNotMatch(readingList(["area:docs"]), /lessons/);
});
