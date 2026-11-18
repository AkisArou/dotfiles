import { test } from "node:test";
import assert from "node:assert/strict";
import { projectFor, serveCommands, serveRoutes } from "./sbn.ts";

const project = (name: string, roots: string[]) => ({
  name,
  directory: `/apps/${name}`,
  roots,
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

test("sbn tailnet serves the console and this machine's hosts on the tailnet; off removes only those", () => {
  const routes = serveRoutes({ webPort: 4310, apiPort: 4300 }, [
    { name: "nts", apiPort: 4311 },
  ]);
  assert.deepEqual(serveCommands(routes, true), [
    ["serve", "--bg", "--set-path", "/", "http://127.0.0.1:4310"],
    [
      "serve",
      "--bg",
      "--set-path",
      "/console/rpc",
      "http://127.0.0.1:4300/rpc",
    ],
    ["serve", "--bg", "--set-path", "/nts/rpc", "http://127.0.0.1:4311/rpc"],
    [
      "serve",
      "--bg",
      "--set-path",
      "/nts/terminals",
      "http://127.0.0.1:4311/terminals",
    ],
  ]);
  assert.deepEqual(
    serveCommands(routes, false).map((command) => command.at(-1)),
    ["off", "off", "off", "off"],
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

test("sbn links makes the missing links, repoints stale ones, and never replaces a folder", async () => {
  const { linkPlan, parseLinks } = await import("./sbn.ts");
  const links = parseLinks(
    JSON.stringify({
      a: "/repos/a",
      b: "/repos/b",
      c: "/repos/c",
      d: "/repos/gone",
    }),
  );
  const here = (name: string) =>
    name === "a"
      ? ({ kind: "missing" } as const)
      : name === "b"
        ? ({ kind: "link", to: "/old/b" } as const)
        : ({ kind: "other" } as const);
  const plan = linkPlan(links, here, (folder) => folder !== "/repos/gone");
  assert.deepEqual(plan.make, [["a", "/repos/a"]]);
  assert.deepEqual(plan.repoint, [["b", "/repos/b"]]);
  assert.deepEqual(plan.blocked, ["c"], "a real folder is the owner's to move");
  assert.deepEqual(
    plan.absent,
    [["d", "/repos/gone"]],
    "a repository not cloned here is reported",
  );
  assert.throws(() => parseLinks("[]"), /maps each project name/);
});

test("sbn links names each linked project's folder in the workspace, and leaves its link out", async () => {
  const { workspaceWithLinks } = await import("./sbn.ts");
  const yaml =
    '# One folder per project.\npackages:\n  - "*"\n\nallowBuilds:\n  esbuild: true\n';
  const linked = workspaceWithLinks(
    yaml,
    { nable: "/home/me/repos/nable/signalbox" },
    "/home/me/dotfiles/signalbox",
  );
  assert.equal(
    linked,
    '# One folder per project.\npackages:\n  - "*"\n  # Kept by sbn links from links.json: each linked project installs from its own folder.\n  - "!nable"\n  - "../../repos/nable/signalbox"\n\nallowBuilds:\n  esbuild: true\n',
  );
  assert.equal(
    workspaceWithLinks(
      linked,
      { nable: "/home/me/repos/nable/signalbox" },
      "/home/me/dotfiles/signalbox",
    ),
    linked,
    "linking again changes nothing",
  );
  assert.equal(
    workspaceWithLinks(linked, {}, "/w"),
    yaml,
    "an unlinked project leaves the list",
  );
});

test("sbn's projects are its folders and its links to folders elsewhere, each with a project.json", async () => {
  const { projects } = await import("./sbn.ts");
  const { mkdtempSync, mkdirSync, rmSync, symlinkSync, writeFileSync } =
    await import("node:fs");
  const { tmpdir } = await import("node:os");
  const { join } = await import("node:path");
  const directory = mkdtempSync(join(tmpdir(), "sbn-projects-"));
  const elsewhere = mkdtempSync(join(tmpdir(), "sbn-repo-"));
  try {
    const manifest = (name: string) =>
      JSON.stringify({
        name,
        title: name,
        roots: [directory],
        apiPort: 1,
        coordinator: false,
      });
    mkdirSync(join(directory, "nts"));
    writeFileSync(join(directory, "nts/project.json"), manifest("nts"));
    writeFileSync(join(elsewhere, "project.json"), manifest("nable"));
    symlinkSync(elsewhere, join(directory, "nable"));
    mkdirSync(join(directory, "console"));
    writeFileSync(join(directory, "hosts.json"), "{}");
    symlinkSync(join(directory, "gone"), join(directory, "dangling"));
    assert.deepEqual(
      projects(directory)
        .map((project) => project.name)
        .sort(),
      ["nable", "nts"],
      "a linked project counts; a folder without project.json, a file and a dangling link do not",
    );
  } finally {
    rmSync(directory, { recursive: true, force: true });
    rmSync(elsewhere, { recursive: true, force: true });
  }
});

test("sbn update pulls each folder's repository once, and nothing for a folder that is not there", async () => {
  const { updateRepositories } = await import("./sbn.ts");
  const tops: Record<string, string> = {
    "/home/me/dotfiles/home/.config/signalbox": "/home/me/dotfiles",
    "/home/me/Projects/signalbox": "/home/me/Projects/signalbox",
    "/home/me/nable/tools/signalbox": "/home/me/nable",
    "/home/me/dotfiles/home/.config/signalbox/other": "/home/me/dotfiles",
  };
  assert.deepEqual(
    updateRepositories(
      [...Object.keys(tops), "/home/me/not-cloned"],
      (folder) => tops[folder],
    ),
    ["/home/me/dotfiles", "/home/me/Projects/signalbox", "/home/me/nable"],
  );
});
