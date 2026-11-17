import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, chmod, rm, stat } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { createServer } from "node:net";
import { startHost, connectClient } from "signalbox/host";
import { ntsConfig } from "../config.ts";
import { createNtsProject, requireCheckout } from "./config.ts";
import type { RemoteReading } from "signalbox/core/remote";
import type { Issue } from "../shared/model.ts";

async function until<T>(read: () => Promise<T>, ready: (value: T) => boolean): Promise<T> {
  const deadline = Date.now() + 10_000;
  while (true) {
    const value = await read();
    if (ready(value)) return value;
    if (Date.now() > deadline) throw new Error("Timed out waiting for project source");
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
}
test("NTS application reads GitHub and plan, exposes real failures and no simulator operations", async () => {
  const directory = await mkdtemp(join(tmpdir(), "signalbox-nts-project-"));
  const root = join(directory, "repo"),
    bin = join(directory, "bin"),
    previousPath = process.env.PATH;
  let host: Awaited<ReturnType<typeof startHost>> | undefined;
  let client: Awaited<ReturnType<typeof connectClient>> | undefined;
  try {
    await mkdir(bin);
    await mkdir(join(root, "docs"), { recursive: true });
    await writeFile(join(root, "docs/plan.md"), "# NTS plan\nExisting work remains in NTS.");
    const rows = [
      {
        number: 807,
        title: "Real issue shape",
        body: "Read from gh",
        url: "https://github.com/AkisArou/nts/issues/807",
        labels: [{ name: "task" }],
        assignees: [],
      },
    ];
    await writeFile(
      join(bin, "gh"),
      `#!${process.execPath}\nconsole.log(${JSON.stringify(JSON.stringify(rows))})\n`,
    );
    await chmod(join(bin, "gh"), 0o755);
    process.env.PATH = `${bin}:${previousPath}`;
    // Free ports of the test's own, never the running host's.
    const freePort = async () => {
      const socket = createServer();
      await new Promise<void>((resolve) => socket.listen(0, "127.0.0.1", resolve));
      const free = (socket.address() as import("node:net").AddressInfo).port;
      await new Promise<void>((resolve) => socket.close(() => resolve()));
      return free;
    };
    const port = await freePort();
    const settings = {
      ...ntsConfig,
      root,
      state: join(directory, "state"),
      apiPort: port,
      webPort: await freePort(),
      discoverTerminals: false,
      issueIntervalMs: 60_000,
    };
    host = await startHost(createNtsProject(settings, { directory: join(directory, "web") }), {
      stateDirectory: settings.state,
    });
    client = await connectClient({ url: `ws://127.0.0.1:${port}/rpc` });
    const connected = client;
    const issues = await until(
      () => connected.request<RemoteReading<Issue[]>>({ type: "read", ref: "nts.issues" }),
      (value) => value.status === "ready",
    );
    assert.equal(issues.status === "ready" && issues.value[0]?.number, 807);
    assert.equal(issues.status === "ready" && issues.value.length, 1);
    const plan = await until(
      () => connected.request<RemoteReading<string>>({ type: "read", ref: "nts.plan" }),
      (value) => value.status === "ready",
    );
    assert.match(plan.status === "ready" ? plan.value : "", /Existing work/);
    const landing = await until(
      () => connected.request<RemoteReading<unknown>>({ type: "read", ref: "nts.landing" }),
      (value) => value.status === "error",
    );
    assert.equal(landing.status, "error", "missing NTS tooling never becomes sample data");
    await assert.rejects(
      connected.request({ type: "call", ref: "lab.control", input: { action: "resume" } }),
      /Unknown endpoint/,
    );
    assert.equal(
      (await connected.request<RemoteReading<unknown[]>>({ type: "read", ref: "nts.agents" }))
        .status,
      "ready",
    );
  } finally {
    if (previousPath === undefined) delete process.env.PATH;
    else process.env.PATH = previousPath;
    client?.close();
    await host?.stop();
    await rm(directory, { recursive: true, force: true });
  }
});
test("missing NTS checkout is actionable and never creates a sample repository", async () => {
  const directory = await mkdtemp(join(tmpdir(), "signalbox-nts-missing-"));
  const root = join(directory, "missing");
  try {
    await assert.rejects(
      requireCheckout({ ...ntsConfig, root }),
      /NTS checkout not found.*NTS_ROOT/,
    );
    assert.equal(await stat(root).catch(() => undefined), undefined);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

test("the host accepts its own web page under each loopback name, or only a configured origin", async () => {
  const { browserOrigins } = await import("./config.ts");
  assert.deepEqual(browserOrigins(4310), [
    "http://127.0.0.1:4310",
    "http://localhost:4310",
    "http://[::1]:4310",
  ]);
  assert.deepEqual(browserOrigins(4310, "https://nts.example"), ["https://nts.example"]);
});
