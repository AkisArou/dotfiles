import { test } from "node:test";
import assert from "node:assert/strict";
import { Cell } from "signalbox/core";
import type { Issue } from "@signalbox/kit/shared/protocol";

test("the settings view counts with the issues last read, so GitHub being down never takes it away", async () => {
  const { lastIssues } = await import("./endpoints.ts");
  const read = new Cell<Issue[]>();
  const view = lastIssues(read);
  assert.deepEqual(view.get(), { status: "ready", value: [], observedAt: 0 }, "not read yet: none");
  const issues = [{ number: 1 }] as unknown as Issue[];
  read.set(issues);
  assert.equal(view.get().status, "ready");
  read.fail(new Error("dial tcp: i/o timeout"));
  const reading = view.get();
  assert.ok(
    reading.status === "ready" && reading.value === issues,
    "a failed read keeps the last list",
  );
});
