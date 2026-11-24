import { test } from "node:test";
import assert from "node:assert/strict";
import { agentEnvironment } from "./agent-environment.ts";

test("agents' PATH drops what asdf's exec of the host's node put ahead of its shims", () => {
  const environment = agentEnvironment({
    ASDF_DATA_DIR: "/home/u/.asdf",
    PATH: [
      "/home/u/.asdf/plugins/nodejs/shims",
      "/home/u/.asdf/installs/nodejs/26.13.1/bin",
      "/home/u/.local/bin",
      "/home/u/.asdf/shims",
      "/home/u/.asdf/plugins/nodejs/bin",
      "/usr/bin",
    ].join(":"),
    ASDF_INSTALL_VERSION: "26.13.1",
  });
  assert.deepEqual(environment.env, {
    PATH: "/home/u/.local/bin:/home/u/.asdf/shims:/home/u/.asdf/plugins/nodejs/bin:/usr/bin",
  });
  assert.deepEqual(environment.unsetEnv, [
    "ASDF_INSTALL_TYPE",
    "ASDF_INSTALL_VERSION",
    "ASDF_INSTALL_PATH",
  ]);
});

test("a host started without asdf's exec keeps its PATH, and one without PATH sets none", () => {
  const PATH = "/home/u/.local/bin:/home/u/.asdf/shims:/usr/bin";
  assert.deepEqual(agentEnvironment({ ASDF_DATA_DIR: "/home/u/.asdf", PATH }).env, { PATH });
  assert.equal(agentEnvironment({}).env, undefined);
});
