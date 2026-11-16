import { test } from "node:test";
import assert from "node:assert/strict";
import { join } from "node:path";
import { ntsStateDirectory } from "./state-directory.ts";

test("NTS state is keyed by the project root, not by where the application lives", () => {
  const stateHome = "/state-home";
  const fresh = ntsStateDirectory({ projectRoot: "/work/nts", stateHome });
  assert.ok(fresh.startsWith(join(stateHome, "signalbox/nts/")));
  assert.equal(ntsStateDirectory({ projectRoot: "/work/nts/", stateHome }), fresh);
  assert.notEqual(ntsStateDirectory({ projectRoot: "/work/another-nts", stateHome }), fresh);
  assert.equal(
    ntsStateDirectory({ projectRoot: "/work/nts", stateHome, override: "/explicit" }),
    "/explicit",
  );
});
