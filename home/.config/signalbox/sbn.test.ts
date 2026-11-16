import { test } from "node:test";
import assert from "node:assert/strict";
import { projectFor } from "./sbn.ts";

const project = (name: string, roots: string[]) => ({ name, directory: `/apps/${name}`, roots, webPort: 1, apiPort: 2 });

test("sbn picks the project whose root contains the directory, deepest root first", () => {
  const all = [project("nts", ["/work/nts", "/cache/nts-pool"]), project("lab", ["/work"]), project("deep", ["/work/nts/examples"])];
  assert.equal(projectFor(all, "/work/nts")?.name, "nts");
  assert.equal(projectFor(all, "/work/nts/compiler/core")?.name, "nts");
  assert.equal(projectFor(all, "/cache/nts-pool/4/tooling")?.name, "nts");
  assert.equal(projectFor(all, "/work/nts/examples/x")?.name, "deep");
  assert.equal(projectFor(all, "/work/other")?.name, "lab");
  assert.equal(projectFor(all, "/work/ntsx")?.name, "lab", "a sibling with a shared prefix is not inside");
  assert.equal(projectFor(all, "/elsewhere"), undefined);
});
