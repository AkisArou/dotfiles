/** @jsxImportSource @opentui/react */
import { test } from "node:test";
import { writeFile } from "node:fs/promises";
import assert from "node:assert/strict";
import { act } from "react";
import { testRender } from "@opentui/react/test-utils";
import { createClient } from "signalbox/client";
import { SignalboxProvider } from "signalbox/client/react";
import { TerminalApp } from "./app.tsx";
import type { Overview, Agent } from "../shared/model.ts";
import { colors } from "./theme.ts";
import { TerminalTheme } from "./selection.tsx";
import { ntsPages, ntsSources } from "../status/model.ts";
import { sampleReports } from "../status/tests/fixtures.ts";
import type { RemoteReading } from "signalbox/core/remote";
import type { Reading } from "../status/model.ts";

const opusHigh = {
  id: "claude/claude-opus-5-5/high",
  label: "Opus 5.5 · high",
  harness: "claude",
  model: "claude-opus-5-5",
  effort: "high",
};
const codexHigh = {
  id: "codex/configured/high",
  label: "Codex (configured model) · high",
  harness: "codex",
  effort: "high",
};
const settingsView = (paused: { reason: string } | null) => ({
  path: "/apps/nts/settings.json",
  version: "v1",
  error: null,
  workerSlots: 2,
  maxRounds: 4,
  roles: {
    coordinator: { pick: "first", models: [opusHigh] },
    worker: { pick: "coordinator", models: [opusHigh] },
    reviewer: { pick: "first", models: [opusHigh] },
  },
  paused,
  unavailable: {},
  catalog: [opusHigh, codexHigh],
});

test("tasks use NTS issue data, fzf filters and workspace-aware lazygit without simulated commands", async () => {
  const calls: string[] = [],
    gitRuns: string[] = [],
    inputs: unknown[] = [];
  let pick: (lines: readonly string[], title: string) => Promise<number | undefined> = async () =>
    undefined;
  let pickMany: (
    lines: readonly string[],
    title: string,
    chosen: readonly number[],
  ) => Promise<number[] | undefined> = async () => undefined;
  const issue = {
    id: "807",
    number: 807,
    title: "Withdrawal cancellation",
    body: "Drain owned work",
    url: "https://github.com/AkisArou/nts/issues/807",
    area: "tooling",
    stream: "Tooling",
    group: "Landing",
    labels: ["task", "area:tooling"],
    state: "in_progress",
    assignees: [],
    owner: "codex-worker",
    blockedBy: [],
    parent: null,
    progress: null,
  };
  const values: Record<string, unknown> = {
    "nts.overview": { name: "nts", root: "/nts", repo: "AkisArou/nts", startedAt: 1 },
    "nts.issues": [
      issue,
      {
        ...issue,
        id: "808",
        number: 808,
        area: "runtime",
        title: "Array ownership",
        state: "held",
      },
    ],
    "nts.settings": settingsView({ reason: "usage low" }),
    "nts.assignments": [],
    "nts.needs": {
      questions: [
        {
          id: "mac",
          revision: 1,
          status: "pending",
          question: "Buy a Mac mini?",
          context: "Only for the macOS host port.",
          issue: { number: 583, title: "Compiler host ports", url: "https://example/583" },
          options: [{ label: "No, leave it unrun", then: "parked" }, { label: "Yes, buy one" }],
          askedBy: "coordinator",
          requestedAt: 1,
          answer: null,
          then: null,
          answeredAt: null,
          posted: null,
        },
      ],
      unasked: [{ number: 777, title: "Share the recipes", url: "https://example/777" }],
      answered: [],
    },
    "nts.agents": [
      {
        id: "worker",
        name: "Codex worker",
        role: "codex",
        state: "running",
        path: "/pool/1",
        issue: 807,
      },
      {
        id: "gone",
        name: "Finished reviewer",
        role: "reviewer",
        state: "ended",
        path: null,
        issue: 807,
      },
    ],
    "nts.plan": "# Plan",
    "nts.terminal-programs": [],
  };
  const client = createClient({
    reconnect: false,
    connect: async (handlers) => ({
      send(request) {
        if (request.type === "subscribe")
          queueMicrotask(() =>
            handlers.message({
              id: request.id,
              result: {
                status: "ready",
                value: values[request.ref!] ?? { root: "/nts", readAt: 1, gauges: [], tables: [] },
                observedAt: 1,
              },
              event: true,
            }),
          );
        if (request.type === "call") {
          calls.push(request.ref!);
          inputs.push(request.input);
          queueMicrotask(() => handlers.message({ id: request.id, result: { revision: 2 } }));
        }
      },
      close() {},
    }),
  });
  const setup = await testRender(
    <SignalboxProvider client={client}>
      <TerminalTheme>
        <TerminalApp
          tools={{
            find: (lines, title) => pick(lines, title),
            findMany: (lines, title, chosen) => pickMany(lines, title, chosen),
            run: async (command, cwd) => {
              // The editor for a written answer: the person writes it into the file.
              const file = command.args?.at(-1);
              if (file?.includes("answer-"))
                return writeFile(file, "Leave it unrun\n# notes stay out\n");
              gitRuns.push(cwd);
            },
          }}
          onExit={() => {}}
        />
      </TerminalTheme>
    </SignalboxProvider>,
    { width: 110, height: 40, backgroundColor: colors.background },
  );
  const keys = async (...names: string[]) => {
    for (const name of names) {
      await act(async () => {
        await setup.mockInput.pressKeys([name]);
      });
      await setup.renderOnce();
    }
  };
  try {
    await act(async () => {
      client.start();
      await new Promise((resolve) => setImmediate(resolve));
    });
    // The Now tab lists live agents; ended ones are a count.
    await keys("1");
    assert.doesNotMatch(setup.captureCharFrame(), /Finished reviewer/);
    assert.match(setup.captureCharFrame(), /Agents: \d+ · 1 ended/);
    await keys("3");
    // Every tab's bottom bar says how it runs, the pause first.
    assert.match(
      setup.captureCharFrame(),
      /AkisArou\/nts · PAUSED: usage low · workers 2 · rounds 4 ·/,
    );
    assert.match(setup.captureCharFrame(), /Withdrawal cancellation/);
    assert.match(setup.captureCharFrame(), /Drain owned work/);
    assert.ok(
      setup
        .captureSpans()
        .lines.flatMap((line) => line.spans)
        .some((span) => span.text.trim() && span.fg.intent === "default"),
    );
    let release!: (value: number | undefined) => void;
    pick = async () =>
      new Promise((resolve) => {
        release = resolve;
      });
    await keys("/");
    await keys("c", "s", "a");
    assert.equal(calls.length, 0);
    await act(async () => {
      release(0);
    });
    pick = async (lines, title) => {
      assert.equal(title, "Filter Tasks");
      return lines.findIndex((line) => line.includes("area: runtime"));
    };
    await keys("f");
    await setup.renderOnce();
    assert.match(setup.captureCharFrame(), /Array ownership/);
    assert.doesNotMatch(setup.captureCharFrame(), /Withdrawal cancellation/);
    await keys("F", "4", "l", "L");
    assert.deepEqual(gitRuns, ["/pool/1", "/nts"]);
    await act(async () => {
      setup.resize(80, 24);
    });
    await keys("3");
    assert.doesNotMatch(setup.captureCharFrame(), /Drain owned work/);
    await act(async () => {
      setup.mockInput.pressEnter();
    });
    await setup.renderOnce();
    assert.match(setup.captureCharFrame(), /Drain owned work/);
    assert.equal(calls.length, 0);
    // Ended sessions are hidden in Agents until `e` shows them.
    await keys("escape", "4");
    assert.doesNotMatch(setup.captureCharFrame(), /Finished reviewer/);
    assert.match(setup.captureCharFrame(), /1 ended hidden \(e\)/);
    // t on an agent shows its task over everything; t again (or Escape) closes it.
    await keys("t");
    assert.match(setup.captureCharFrame(), /#807 · Escape or t closes/);
    assert.match(setup.captureCharFrame(), /Withdrawal cancellation/);
    await keys("t");
    assert.doesNotMatch(setup.captureCharFrame(), /Escape or t closes/);
    await keys("e");
    assert.match(setup.captureCharFrame(), /Finished reviewer/);
    // The Agents tab ends with the settings; Enter offers a row's values in the finder.
    await keys("e");
    await act(async () => {
      setup.resize(110, 40);
    });
    await setup.renderOnce();
    assert.match(setup.captureCharFrame(), /Settings · Enter changes/);
    assert.match(setup.captureCharFrame(), /Worker slots\s+2/);
    let offered: { lines: readonly string[]; title: string } | undefined;
    pick = async (lines, title) => {
      offered = { lines, title };
      return lines.findIndex((line) => line.endsWith(" 3 worker slots"));
    };
    await keys("G", "k", "k", "k", "k", "k", "k", "k", "k", "k");
    await act(async () => {
      setup.mockInput.pressEnter();
      await new Promise((resolve) => setImmediate(resolve));
    });
    assert.equal(offered?.title, "Worker slots");
    assert.ok(offered?.lines.includes("✓ 2 worker slots"), "the current value is marked");
    assert.equal(calls.at(-1), "nts.settings.set");
    assert.deepEqual(inputs.at(-1), { workerSlots: 3, version: "v1" });
    // A role's models row offers every model at every effort, the listed ones ticked;
    // the ticked lines come back in the order ticked.
    let many: { lines: readonly string[]; title: string; chosen: readonly number[] } | undefined;
    pickMany = async (lines, title, chosen) => {
      many = { lines, title, chosen };
      return [...chosen, lines.findIndex((line) => line.startsWith("Codex"))];
    };
    await keys("j", "j", "j", "j");
    await act(async () => {
      setup.mockInput.pressEnter();
      await new Promise((resolve) => setImmediate(resolve));
    });
    assert.equal(many?.title, "Worker models");
    assert.deepEqual(many?.chosen, [0], "the listed model starts ticked");
    assert.deepEqual(inputs.at(-1), {
      roles: { worker: { models: ["claude/claude-opus-5-5/high", "codex/configured/high"] } },
      version: "v1",
    });
    // How one is picked is its own row.
    pick = async (lines, title) => {
      offered = { lines, title };
      return lines.findIndex((line) => line.includes("spread:"));
    };
    await keys("j");
    await act(async () => {
      setup.mockInput.pressEnter();
      await new Promise((resolve) => setImmediate(resolve));
    });
    assert.equal(offered?.title, "Worker pick");
    assert.ok(offered?.lines.some((line) => line.startsWith("✓ coordinator:")));
    assert.deepEqual(inputs.at(-1), { roles: { worker: { pick: "spread" } }, version: "v1" });
    // Needs you is tab 2: the question's choices come first; answering one sends that option.
    const answered = async (count: number) => {
      for (let i = 0; i < 100 && calls.length < count; i++)
        await act(async () => {
          await new Promise((resolve) => setTimeout(resolve, 20));
        });
    };
    await keys("escape", "2");
    assert.match(setup.captureCharFrame(), /Questions · 1/);
    assert.match(setup.captureCharFrame(), /Not asked yet · 1/);
    pick = async (lines, title) => {
      offered = { lines, title };
      return 0;
    };
    let before = calls.length;
    await act(async () => {
      setup.mockInput.pressEnter();
    });
    await answered(before + 1);
    assert.ok(offered?.lines.includes("1. No, leave it unrun  → parked (waits on your word)"));
    assert.equal(offered?.lines.at(-1), "✎ Write a different answer");
    assert.equal(calls.at(-1), "nts.needs.answer");
    assert.deepEqual(inputs.at(-1), { id: "mac", revision: 1, option: 0 });
    // An issue nobody asked about: the answer is written in the editor, then where it goes.
    pick = async (lines, title) => {
      offered = { lines, title };
      return lines.findIndex((line) => line.includes("parked"));
    };
    await keys("j");
    before = calls.length;
    await act(async () => {
      setup.mockInput.pressEnter();
    });
    await answered(before + 1);
    assert.equal(offered?.title, "Then #777");
    assert.equal(calls.at(-1), "nts.needs.answer-issue");
    assert.deepEqual(inputs.at(-1), { issue: 777, answer: "Leave it unrun", then: "parked" });
  } finally {
    await act(async () => {
      client.close();
      setup.renderer.destroy();
    });
  }
});

test("Vim motions follow the focused pane, preserve selections and keep one live Landing even when its reader fails", async () => {
  const calls: string[] = [],
    gitRuns: string[] = [];
  const overview: Overview = { name: "nts", root: "/nts", repo: "AkisArou/nts", startedAt: 1 };
  const agents: Agent[] = Array.from({ length: 50 }, (_, index) => ({
    id: `worker/${index}`,
    name: `Agent ${String(index).padStart(2, "0")}`,
    role: "compiler",
    state: "idle",
    path: `/nts/worker-${index}`,
    issue: null,
  }));
  const values: Record<string, unknown> = {
    "nts.overview": overview,
    "nts.issues": [],
    "nts.settings": settingsView(null),
    "nts.assignments": [],
    "nts.needs": { questions: [], unasked: [], answered: [] },
    "nts.agents": agents,
    "nts.plan": "# Plan",
  };
  const samples = sampleReports();
  for (const page of ntsPages)
    values[ntsSources[page].id] = {
      ...samples[page],
      root: "/nts",
      readAt: 1,
    } satisfies Reading;
  values[ntsSources.Benchmarks.id] = {
    root: "/nts",
    readAt: 1,
    gauges: [],
    tables: [
      {
        id: "long",
        title: "Long evidence",
        key: "name",
        columns: [
          { key: "name", label: "case" },
          { key: "body", label: "details" },
        ],
        rows: Array.from({ length: 200 }, (_, index) => ({
          name: `case ${String(index).padStart(3, "0")}`,
          body: Array.from(
            { length: 60 },
            (_, line) => `evidence line ${String(line).padStart(2, "0")}`,
          ).join("\n"),
        })),
      },
    ],
  } satisfies Reading;
  let updateLanding!: (reading: RemoteReading<Reading>) => void;
  const client = createClient({
    reconnect: false,
    connect: async (handlers) => ({
      send(request) {
        if (request.type === "subscribe") {
          if (request.ref === ntsSources.Landing.id)
            updateLanding = (reading) =>
              handlers.message({ id: request.id, result: reading, event: true });
          queueMicrotask(() =>
            handlers.message({
              id: request.id,
              result: { status: "ready", value: values[request.ref!], observedAt: 1 },
              event: true,
            }),
          );
        }
        if (request.type === "call") {
          calls.push(request.ref!);
          queueMicrotask(() => handlers.message({ id: request.id, result: { accepted: true } }));
        }
      },
      close() {},
    }),
  });
  const setup = await testRender(
    <SignalboxProvider client={client}>
      <TerminalTheme>
        <TerminalApp
          onExit={() => {}}
          tools={{
            find: async () => undefined,
            findMany: async () => undefined,
            run: async (_command, cwd) => {
              gitRuns.push(cwd);
            },
          }}
        />
      </TerminalTheme>
    </SignalboxProvider>,
    { width: 80, height: 24, backgroundColor: colors.background, kittyKeyboard: true },
  );
  const keys = async (...names: string[]) => {
    for (const name of names) {
      await act(async () => {
        if (name === "return") setup.mockInput.pressEnter();
        else if (name === "escape") setup.mockInput.pressEscape();
        else await setup.mockInput.pressKeys([name]);
      });
      await setup.renderOnce();
    }
  };
  const ctrl = async (name: string) => {
    await act(async () => {
      setup.mockInput.pressKey(name, { ctrl: true });
    });
    await setup.renderOnce();
  };
  const selectedCase = async () => {
    await keys("return");
    const selected = /case (\d{3})/.exec(setup.captureCharFrame())?.[1];
    await keys("escape");
    return Number(selected);
  };
  try {
    await act(async () => {
      client.start();
      await new Promise((resolve) => setImmediate(resolve));
    });
    await setup.renderOnce();
    await keys("7", "G");
    assert.equal(await selectedCase(), 199, "G reaches the last item without opening lazygit");
    await keys("g", "g");
    assert.equal(await selectedCase(), 0);
    await act(async () => {
      await setup.mockInput.typeText("jjj");
    });
    await setup.renderOnce();
    assert.equal(await selectedCase(), 3, "quick repeated keys accumulate movement");
    await keys("g", "g");
    await ctrl("d");
    const half = await selectedCase();
    assert.ok(half >= 6 && half <= 10, "Ctrl-D moves half the actual viewport");
    await ctrl("u");
    assert.equal(await selectedCase(), 0);
    await ctrl("f");
    assert.ok((await selectedCase()) > half, "Ctrl-F moves a full pane");
    await ctrl("b");
    assert.equal(await selectedCase(), 0);
    await ctrl("j");
    assert.equal(await selectedCase(), 0, "modified j must not act as plain j");
    await keys("j", "j", "g", "t");
    assert.match(setup.captureCharFrame(), /8 Conformance/);
    await keys("g", "T");
    assert.equal(await selectedCase(), 2, "tab changes retain each view's selection");

    await keys("return");
    const top = setup.captureCharFrame();
    await ctrl("d");
    assert.notEqual(setup.captureCharFrame(), top, "Ctrl-D scrolls details when they have focus");
    await keys("G");
    assert.match(setup.captureCharFrame(), /evidence line 59/);
    await keys("g", "g");
    assert.equal(setup.captureCharFrame(), top, "gg returns to the top of details");
    await keys("escape", "4");
    await ctrl("d");
    await keys("return");
    const agentIndex = Number(/Agent (\d{2})/.exec(setup.captureCharFrame())?.[1]);
    assert.ok(
      agentIndex > 0 && agentIndex < half,
      "two-line agents move fewer items per half pane",
    );
    await keys("escape", "g", "3", "j");
    assert.equal(gitRuns.length, 0, "unrelated keys cancel the g prefix without running git");

    await keys("5");
    assert.match(setup.captureCharFrame(), /NTS landing service/);
    assert.equal((setup.captureCharFrame().match(/Landing/g) ?? []).length, 1, "one Landing tab");
    await keys("y", "n", "l", "s", "c");
    assert.equal(calls.length, 0, "live Landing never invokes workflow actions");
    await act(async () => {
      updateLanding({
        status: "error",
        error: { code: "unavailable", message: "NTS status unavailable" },
      });
    });
    await setup.renderOnce();
    assert.match(setup.captureCharFrame(), /NTS status unavailable/);
    assert.doesNotMatch(setup.captureCharFrame(), /No item selected/);
    await keys("y", "l");
    assert.equal(
      calls.length,
      0,
      "a failed live reader must not invent a different landing workflow",
    );
  } finally {
    await act(async () => {
      client.close();
      await setup.dispose();
    });
  }
});
