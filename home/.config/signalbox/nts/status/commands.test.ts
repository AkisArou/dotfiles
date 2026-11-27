// nts tooling runs on the repository's pinned Node, never this host's own.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { pinnedNode } from "./commands.ts";

test("the pinned Node is the directory's, kept while its pin reads the same, and asked again when it changes", async (t) => {
  const repo = await mkdtemp(join(tmpdir(), "nts-pinned-node-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await writeFile(join(repo, ".tool-versions"), "nodejs 26.13.0\n");
  const asked: string[] = [];
  const resolve = async (cwd: string) => {
    asked.push(cwd);
    return `/asdf/nodejs/${asked.length}/bin/node`;
  };
  assert.equal(await pinnedNode(repo, resolve), "/asdf/nodejs/1/bin/node");
  assert.equal(await pinnedNode(repo, resolve), "/asdf/nodejs/1/bin/node", "kept");
  await writeFile(join(repo, ".tool-versions"), "nodejs 26.14.0\n");
  assert.equal(await pinnedNode(repo, resolve), "/asdf/nodejs/2/bin/node", "a new pin asks again");
  assert.deepEqual(asked, [repo, repo]);
  assert.notEqual(await pinnedNode(repo, resolve), process.execPath, "never the host's own Node");
});

test("a pin asdf cannot serve fails, named, and is asked again next time", async (t) => {
  const repo = await mkdtemp(join(tmpdir(), "nts-pinned-node-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await writeFile(join(repo, ".tool-versions"), "nodejs 99.0.0\n");
  let calls = 0;
  const failing = async () => {
    calls++;
    throw new Error("asdf which node: version 99.0.0 is not installed");
  };
  await assert.rejects(pinnedNode(repo, failing), /99\.0\.0 is not installed/);
  await assert.rejects(pinnedNode(repo, failing), /not installed/);
  assert.equal(calls, 2, "a failure is not kept");
});
