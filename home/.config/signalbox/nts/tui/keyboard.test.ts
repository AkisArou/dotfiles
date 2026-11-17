import { test } from "node:test";
import assert from "node:assert/strict";
import { popupKey } from "./keyboard.ts";

const key = (name: string, change: { ctrl?: boolean; shift?: boolean } = {}) => ({
  name,
  ctrl: false,
  meta: false,
  option: false,
  shift: false,
  ...change,
});

test("the task popup scrolls like a pane: gg goes to the top, G to the bottom", () => {
  const g = popupKey(key("g"), false, "t");
  assert.deepEqual(g, { afterG: true, action: { kind: "none" } });
  assert.deepEqual(popupKey(key("g"), g.afterG, "t").action, { kind: "edge", last: false });
  assert.deepEqual(popupKey(key("g", { shift: true }), false, "t").action, {
    kind: "edge",
    last: true,
  });
  assert.deepEqual(popupKey(key("j"), false, "t").action, {
    kind: "scroll",
    direction: 1,
    distance: "line",
  });
  assert.deepEqual(popupKey(key("d", { ctrl: true }), false, "t").action, {
    kind: "scroll",
    direction: 1,
    distance: "half",
  });
  assert.deepEqual(popupKey(key("f", { ctrl: true }), false, "t").action, {
    kind: "scroll",
    direction: 1,
    distance: "page",
  });
});

test("Escape, q and the key that opened a popup close it; gt does not", () => {
  for (const name of ["escape", "t", "q"])
    assert.deepEqual(popupKey(key(name), false, "t").action, { kind: "close" });
  assert.deepEqual(popupKey(key("t"), true, "t").action, { kind: "none" });
  assert.deepEqual(popupKey(key("e"), false, "e").action, { kind: "close" });
  assert.deepEqual(popupKey(key("e"), false, "t").action, { kind: "none" });
  assert.equal(popupKey(key("x"), true, "t").afterG, false, "another key cancels the prefix");
});
