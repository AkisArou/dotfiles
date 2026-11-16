import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { ntsStateDirectory } from "./state-directory.ts";

test("NTS state survives the package promotion and fresh state does not depend on the source checkout", async () => {
  const directory = await mkdtemp(join(tmpdir(), "signalbox-state-location-"));
  const appDirectory = join(directory, "signalbox/playground");
  const previous = join(directory, "signalbox/v2/playground/.signalbox/nts");
  const stateHome = join(directory, "state-home");
  const projectRoot = join(directory, "nts");
  const options = { appDirectory, projectRoot, stateHome };
  try {
    const fresh = ntsStateDirectory(options);
    assert.ok(fresh.startsWith(join(stateHome, "signalbox/nts/")));
    assert.equal(
      ntsStateDirectory({
        ...options,
        appDirectory: join(directory, "another-checkout/playground"),
      }),
      fresh,
    );
    assert.notEqual(
      ntsStateDirectory({ ...options, projectRoot: join(directory, "another-nts") }),
      fresh,
    );
    await mkdir(previous, { recursive: true });
    assert.equal(
      ntsStateDirectory(options),
      fresh,
      "an empty historical directory is not live state",
    );
    await writeFile(join(previous, "runtime.sqlite"), "existing execution receipts");
    assert.equal(
      ntsStateDirectory(options),
      previous,
      "keep the exact previous path and therefore its tmux socket identity",
    );
    assert.equal(
      ntsStateDirectory({ ...options, override: join(directory, "explicit") }),
      join(directory, "explicit"),
    );
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
