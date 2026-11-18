import { test } from "node:test";
import assert from "node:assert/strict";
import { projectFor, serveCommands } from "./sbn.ts";

const project = (name: string, roots: string[]) => ({
  name,
  directory: `/apps/${name}`,
  roots,
  webPort: 1,
  apiPort: 2,
});

test("sbn picks the project whose root contains the directory, deepest root first", () => {
  const all = [
    project("nts", ["/work/nts", "/cache/nts-pool"]),
    project("lab", ["/work"]),
    project("deep", ["/work/nts/examples"]),
  ];
  assert.equal(projectFor(all, "/work/nts")?.name, "nts");
  assert.equal(projectFor(all, "/work/nts/compiler/core")?.name, "nts");
  assert.equal(projectFor(all, "/cache/nts-pool/4/tooling")?.name, "nts");
  assert.equal(projectFor(all, "/work/nts/examples/x")?.name, "deep");
  assert.equal(projectFor(all, "/work/other")?.name, "lab");
  assert.equal(
    projectFor(all, "/work/ntsx")?.name,
    "lab",
    "a sibling with a shared prefix is not inside",
  );
  assert.equal(projectFor(all, "/elsewhere"), undefined);
});

test("sbn tailnet serves the page, its API and its terminals on the tailnet; off removes only those", () => {
  const ports = { webPort: 4310, apiPort: 4311 };
  assert.deepEqual(serveCommands(ports, true), [
    ["serve", "--bg", "--set-path", "/", "http://127.0.0.1:4310"],
    ["serve", "--bg", "--set-path", "/rpc", "http://127.0.0.1:4311/rpc"],
    [
      "serve",
      "--bg",
      "--set-path",
      "/terminals",
      "http://127.0.0.1:4311/terminals",
    ],
  ]);
  assert.deepEqual(
    serveCommands(ports, false).map((command) => command.at(-1)),
    ["off", "off", "off"],
  );
});

test("a machine runs exactly the hosts hosts.json gives it: start what is missing, stop the rest", async () => {
  const { hostPlan, parseHosts } = await import("./sbn.ts");
  const known = ["nts", "nable"];
  assert.deepEqual(
    hostPlan(["nts"], { enabled: ["nts"], active: ["nts"] }, known),
    {
      enable: [],
      disable: [],
      unknown: [],
    },
  );
  assert.deepEqual(
    hostPlan(["nable"], { enabled: ["nts"], active: ["nts"] }, known),
    {
      enable: ["nable"],
      disable: ["nts"],
      unknown: [],
    },
  );
  assert.deepEqual(
    hostPlan(["nts", "nable"], { enabled: ["nts"], active: [] }, known).enable,
    ["nts", "nable"],
    "an enabled host that is not running is started",
  );
  assert.deepEqual(
    hostPlan(["nts", "lab"], { enabled: [], active: [] }, known).unknown,
    ["lab"],
  );
  assert.deepEqual(
    parseHosts('{"arch-desktop":["nts"],"arch-laptop":["nable"]}'),
    {
      "arch-desktop": ["nts"],
      "arch-laptop": ["nable"],
    },
  );
  assert.throws(
    () => parseHosts('{"arch-desktop":"nts"}'),
    /maps each hostname to a list/,
  );
});
