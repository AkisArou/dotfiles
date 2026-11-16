import { test } from "node:test";
import assert from "node:assert/strict";
import { tabLabels } from "./navigation.ts";
import { tabs, tabKey } from "../shared/model.ts";
test("all NTS views remain visible at 80 columns and number shortcuts stop at 0", () => {
  const labels = tabLabels("Conformance", 78, () => undefined);
  assert.equal(labels.length, tabs.length);
  assert.ok(labels.join(" ").length <= 78);
  assert.match(labels[tabs.indexOf("Conformance")]!, /Conformance/);
  assert.equal(tabKey(9), "0");
  assert.equal(tabKey(10), "");
});
