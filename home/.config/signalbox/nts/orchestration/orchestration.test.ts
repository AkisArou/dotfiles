import { test } from "node:test";
import assert from "node:assert/strict";
import { cardPaths } from "@signalbox/kit/orchestration/task-model";
import { PHASE_BUDGET, PROGRESS_GRACE_MS, wakeups } from "./wakeups.ts";
import type { Assignment } from "signalbox/ext/assignments";

test("a card's Paths line names the files a task reserves, and nothing else", () => {
  const body = [
    "**Goal:** something",
    "**Paths:** tooling/gate/disk-capacity.ts, tooling/gate/step-result.ts, the full check's verdict owner under tooling/full-check/, docs/workflow.md, their tests",
    "**Done when:** x",
  ].join("\n");
  assert.deepEqual(cardPaths(body), [
    "docs/workflow.md",
    "tooling/full-check",
    "tooling/gate/disk-capacity.ts",
    "tooling/gate/step-result.ts",
  ]);
  assert.deepEqual(cardPaths("no paths line"), []);
});

const task = (patch: object, assignment: Partial<Assignment> = {}): Assignment =>
  ({
    id: "a1",
    key: "issue/812",
    route: "nts/task",
    acceptedAt: 0,
    input: { issue: 812, instructions: "" },
    status: "active",
    heldResources: [],
    state: {
      phase: "design",
      title: "t",
      revision: "r",
      paths: [],
      tree: null,
      branch: null,
      worker: null,
      reviewer: null,
      design: "",
      commit: null,
      summary: "",
      rounds: 0,
      landedMain: null,
      help: null,
      outcome: null,
      ...patch,
    },
    ...assignment,
  }) as unknown as Assignment;

test("wake-ups have stable identities derived from what they announce", () => {
  const observe = (assignments: Assignment[]) =>
    wakeups({ assignments, landing: undefined, fullCheck: undefined, decided: [], now: 0 }).map(
      (event) => event.id,
    );
  const design = observe([task({ phase: "approve", design: "fix it at the cause" })]);
  assert.equal(design.length, 1);
  assert.match(design[0]!, /^design\/a1\//);
  // The same design observed again is the same event; a revised design is a new one.
  assert.deepEqual(observe([task({ phase: "approve", design: "fix it at the cause" })]), design);
  assert.notDeepEqual(observe([task({ phase: "approve", design: "a better design" })]), design);
  // A stalled agent is announced once per watched report; a phase past its budget once per entry.
  const watch = { key: "build/claude/x/0", idleSince: 0, until: null, nudges: 2, escalated: true };
  assert.deepEqual(observe([task({ phase: "build", stalled: "idle", watch })]), [
    "stalled/a1/build/claude/x/0",
  ]);
  const late = (now: number, phaseSince: number) =>
    wakeups({
      assignments: [task({ phase: "build", phaseSince })],
      landing: undefined,
      fullCheck: undefined,
      decided: [],
      now,
    }).map((event) => event.id);
  assert.deepEqual(late(PHASE_BUDGET.build!, 0), []);
  assert.deepEqual(late(PHASE_BUDGET.build! + 1, 0), ["overdue/a1/build/0"]);
  // An agent's own progress estimate long past is news once, whatever its state.
  const overrun = (now: number) =>
    wakeups({
      assignments: [
        task({ phase: "build", progress: { note: "inner.ts runs", at: 7, until: 1_000 } }),
      ],
      landing: undefined,
      fullCheck: undefined,
      decided: [],
      now,
    }).map((event) => event.id);
  assert.deepEqual(overrun(1_000 + PROGRESS_GRACE_MS), []);
  assert.deepEqual(overrun(1_000 + PROGRESS_GRACE_MS + 1), ["overrun/a1/7"]);
  assert.deepEqual(observe([task({ help: "which owner?" })]).length, 1);
  // An escalated task is announced once per escalation; the next escalation is news again.
  const escalated = (escalation: string) =>
    observe([task({ phase: "escalated", rounds: 5, escalation })]);
  assert.equal(escalated("Escalated after 5 rounds: the review asks again").length, 1);
  assert.match(
    escalated("Escalated after 5 rounds: the review asks again")[0]!,
    /^escalated\/a1\//,
  );
  assert.deepEqual(
    escalated("Escalated after 5 rounds: the review asks again"),
    escalated("Escalated after 5 rounds: the review asks again"),
  );
  assert.notDeepEqual(
    escalated("Escalated after 6 rounds: the landing refused"),
    escalated("Escalated after 5 rounds: the review asks again"),
  );
  assert.deepEqual(
    observe([task({ phase: "done", outcome: "verified" }, { status: "finished" })]),
    ["finished/a1"],
  );
});

test("each area's reading list names workflow sections and lessons that exist", async () => {
  const { readFile, access } = await import("node:fs/promises");
  const { readingList } = await import("./briefs.ts");
  const { ntsConfig } = await import("../config.ts");
  const workflow = await readFile(`${ntsConfig.root}/docs/workflow.md`, "utf8");
  const headings = new Set([...workflow.matchAll(/^## (.+)$/gm)].map((match) => match[1]!));
  for (const area of [
    "tooling",
    "compiler",
    "runtime",
    "jvm",
    "wasm",
    "node",
    "benchmarks",
    "chromium",
    "docs",
  ]) {
    const list = readingList([`area:${area}`]);
    for (const [, name] of list.matchAll(/"## ([^"]+)"/g))
      assert.ok(headings.has(name!), `${area}: no section ${name}`);
    const lessons = /^- docs\/contributing\/lessons\/: (.+)\.$/m.exec(list)?.[1] ?? "";
    for (const [, topic] of lessons.matchAll(/(\w+)\.md/g))
      await access(`${ntsConfig.root}/docs/contributing/lessons/${topic}.md`);
  }
  assert.match(readingList(["area:tooling"]), /concurrency\.md, fixtures\.md, machine\.md/);
  assert.doesNotMatch(readingList(["area:docs"]), /lessons/);
});

test("a task's worker and reviewer work in its tree; the coordinator in the project root", async () => {
  const { managedAgentViews } = await import("./endpoints.ts");
  const { initialTaskState } = await import("@signalbox/kit/orchestration/task-model");
  const task = (issue: number, worker: string | null, tree: string, acceptedAt: number) =>
    ({
      route: "nts/task",
      input: { issue },
      acceptedAt,
      state: {
        ...initialTaskState(),
        tree,
        worker: worker
          ? { id: worker, profile: { harness: "claude", model: "claude-opus-5-5", effort: "high" } }
          : null,
      },
    }) as never;
  const views = managedAgentViews(
    [
      { id: "claude/w", provider: "claude", label: "#802 worker", state: "busy" },
      { id: "claude/old", provider: "claude", label: "#802 worker", state: "ended" },
      {
        id: "claude/c",
        provider: "claude",
        label: "NTS coordinator (generation 5)",
        state: "idle",
      },
      { id: "claude/x", provider: "claude", label: "#9 worker", state: "ended" },
    ],
    [task(802, "claude/w", "/pool/4", 2), task(802, null, "/pool/1", 1)],
    "/nts",
  );
  assert.deepEqual(
    views.map((view) => view.path),
    ["/pool/4", "/pool/4", "/nts", null],
    "its own task's tree, else the issue's latest; no task, no path",
  );
});

test("managed sessions become Agents rows: role and issue from the label, state as reported", async () => {
  const { managedAgentViews } = await import("./endpoints.ts");
  const views = managedAgentViews([
    { id: "claude/a", provider: "claude", label: "#802 worker", state: "busy" },
    { id: "claude/b", provider: "claude", label: "#816 review abc", state: "idle" },
    { id: "claude/c", provider: "claude", label: "#9 worker", state: "ended" },
    { id: "codex/x", provider: "codex", label: "nts coordinator", state: "running" },
  ]);
  assert.deepEqual(
    views.map((view) => [view.name, view.role, view.state, view.issue]),
    [
      ["#802 worker", "worker", "busy", 802],
      ["#816 review abc", "reviewer", "idle", 816],
      ["#9 worker", "worker", "ended", 9],
      ["nts coordinator", "coordinator", "running", null],
    ],
  );
});

test("priorities: set, clear and rank issues; higher urgency ranks lower (goes first)", async () => {
  const { priorities } = await import("@signalbox/kit/orchestration/priorities");
  const { sqliteWorkflowState } = await import("signalbox/ext/integrations/sqlite");
  const { defineConfig, startExtensions } = await import("signalbox/core");
  const { mkdtemp, rm } = await import("node:fs/promises");
  const { tmpdir } = await import("node:os");
  const { join } = await import("node:path");
  const directory = await mkdtemp(join(tmpdir(), "nts-priorities-"));
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const store = priorities({ project: "nts", state: state.state });
  const app = await startExtensions(
    defineConfig({
      id: "p",
      root: directory,
      gate: { accept: () => false },
      extensions: [state.extension, store.extension],
    }),
  );
  try {
    const service = app.context.get(store.service);
    assert.equal(store.rank(823), -0);
    assert.deepEqual(await service.set(823, 2), { "823": 2 });
    assert.equal(store.rank(823), -2);
    assert.equal(store.rank(821), -0, "an unset issue keeps the default");
    assert.deepEqual(await service.set(823, 0), {}, "0 clears it");
    await assert.rejects(service.set(823, 10), /too_big|less than or equal/i);
  } finally {
    await app.stop();
    await rm(directory, { recursive: true, force: true });
  }
});

test("assigning an issue filed a moment ago reads the issues first; a known one does not", async () => {
  const { knownIssue } = await import("./tools.ts");
  const { Cell } = await import("signalbox/core");
  const issues = new Cell([{ number: 824 }] as never[]);
  let refreshed = 0;
  const refresh = async () => void refreshed++;
  await knownIssue(issues, refresh, 824);
  assert.equal(refreshed, 0, "a known issue needs no read");
  await knownIssue(issues, refresh, 825);
  assert.equal(refreshed, 1, "an unknown one is read before the task is submitted");
});
