import { test } from "node:test";
import assert from "node:assert/strict";
import { lutimes, mkdir, mkdtemp, readdir, rm, symlink, utimes, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { removeAged, WEEK_MS } from "./agent-temp.ts";

test("aged temporary files go, then the folders they leave empty; fresh ones and the root stay", async () => {
  const root = await mkdtemp(join(tmpdir(), "nts-agent-temp-"));
  const outside = await mkdtemp(join(tmpdir(), "nts-agent-temp-outside-"));
  try {
    const now = Date.now();
    const old = new Date(now - WEEK_MS - 60_000);
    const write = async (path: string, aged: boolean) => {
      await mkdir(join(root, path, ".."), { recursive: true });
      await writeFile(join(root, path), "x");
      if (aged) await utimes(join(root, path), old, old);
    };
    await write("ended/session/scratchpad/capture.jsonl", true);
    await write("ended/session/tasks/b1.output", true);
    await write("live/session/scratchpad/notes.md", false);
    await write("live/session/tasks/old.output", true);
    await writeFile(join(outside, "keep.txt"), "kept");
    // An aged link goes as a file; what it points to is never touched.
    await symlink(outside, join(root, "live/link"));
    await lutimes(join(root, "live/link"), old, old);
    for (const folder of [
      "ended/session/scratchpad",
      "ended/session/tasks",
      "ended/session",
      "ended",
    ])
      await utimes(join(root, folder), old, old);
    // A fresh empty folder is a session starting: it stays.
    await mkdir(join(root, "starting"));

    const removed = await removeAged(root, WEEK_MS, now);

    assert.deepEqual(removed, { files: 4, folders: 4 });
    assert.deepEqual((await readdir(root)).sort(), ["live", "starting"]);
    assert.deepEqual(await readdir(join(root, "live/session/scratchpad")), ["notes.md"]);
    assert.deepEqual(await readdir(join(root, "live/session/tasks")), []);
    assert.deepEqual(await readdir(join(root, "live")), ["session"]);
    assert.deepEqual(await readdir(outside), ["keep.txt"], "a link is never followed");
  } finally {
    await rm(root, { recursive: true, force: true });
    await rm(outside, { recursive: true, force: true });
  }
});
