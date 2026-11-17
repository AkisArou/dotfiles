// The nts/task route end to end against a throwaway repository whose pool and
// landing commands are small stand-ins, with stand-in agents. It exercises the
// real assignment service, workflow engine, effects and reports.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, rm, writeFile, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { capability, Cell, defineConfig, provide, startExtensions } from "signalbox/core";
import type { Readable } from "signalbox/core";
import {
  agentProfile,
  agentProgram,
  agentRuntime,
  inputText,
  messaging,
  terminate,
} from "signalbox/ext/agents";
import type { AgentInstance } from "signalbox/ext/agents";
import { assignmentRequest, assignmentService } from "signalbox/ext/assignments";
import type { Assignment } from "signalbox/ext/assignments";
import { openWorkflowSqlite } from "signalbox/ext/integrations/openworkflow";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { Issue } from "../shared/model.ts";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import type { ManagedAgents } from "./agents.ts";
import { profiles } from "./profiles.ts";
import { settingsFile } from "./settings.ts";
import { taskRoute } from "./task.ts";
import { taskState } from "./task-model.ts";
import { STALL } from "./stall.ts";

const run = promisify(execFile);
const git = (cwd: string, ...args: string[]) =>
  run("git", ["-c", "user.name=Test", "-c", "user.email=test@example.invalid", ...args], { cwd });

// pool.ts: `take --issue N` adds a worktree on branch N; `status --json` lists it.
const POOL = `import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { join } from "node:path";
const [command, ...rest] = process.argv.slice(2);
const root = process.cwd(), tree = (n) => join(root, "..", "tree-" + n);
const trees = () => execFileSync("git", ["worktree", "list", "--porcelain"], { encoding: "utf8" })
  .split("\\n\\n").filter((block) => block.includes("branch refs/heads/") && !block.includes("refs/heads/main"))
  .map((block) => ({ name: "t", path: block.match(/^worktree (.*)$/m)[1], state: "held",
    task: block.match(/branch refs\\/heads\\/(.*)$/m)[1], ahead: 0, behind: 0, uncommitted: 0, idle_ms: 0, target_bytes: null }));
if (command === "status") console.log(JSON.stringify({ version: 1, pool: root, room: { ok: true, free_bytes: 1, floor_bytes: 0, unbuilt: 0, why: null }, trees: trees() }));
else if (command === "take") { const n = rest[1]; if (!existsSync(tree(n))) execFileSync("git", ["worktree", "add", "-b", n, tree(n), "main"]); }
`;
// check.ts: `<sha> --no-wait` records the submission; `status --json` shows it waiting.
const CHECK = `import { readFileSync, writeFileSync, existsSync } from "node:fs";
const file = process.env.STAND_IN_QUEUE, [first] = process.argv.slice(2);
const queue = existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : [];
if (first === "status") console.log(JSON.stringify({ version: 1, kind: "landing", service: { running: true, state: "idle", since: "", message: null }, checking: null,
  waiting: queue.map((sha) => ({ id: sha.slice(0, 12), sha, branch: null, issue: null, by: "t", submitted_at: "", note: null })),
  held: [], day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null }, recent: [], main: { commit: null, pushed: null } }));
else if (first === "withdraw") { writeFileSync(file, JSON.stringify(queue.filter((sha) => sha !== process.argv[3]))); }
else { queue.push(first); writeFileSync(file, JSON.stringify(queue)); }
`;

test("a task goes from tree to verified through design approval, review and landing", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-task-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const root = join(directory, "repo");
  await mkdir(join(root, "tooling/landing"), { recursive: true });
  await writeFile(join(root, "tooling/landing/pool.ts"), POOL);
  await writeFile(join(root, "tooling/landing/check.ts"), CHECK);
  await git(directory, "init", "-b", "main", root);
  await git(root, "add", ".");
  await git(root, "commit", "-m", "initial");
  process.env.STAND_IN_QUEUE = join(directory, "queue.json");

  const messages: { to: string; text: string }[] = [];
  const started: { id: string; cwd: string; brief: string }[] = [];
  const stopped: string[] = [];
  const instances = new Cell<readonly AgentInstance[]>([]);
  const byRequest = new Map<string, AgentInstance>();
  const fake = agentProgram({
    id: "fake",
    connect: () => ({
      instances,
      async create(request) {
        let instance = byRequest.get(request.requestId);
        if (!instance) {
          instance = {
            id: `fake/${byRequest.size}`,
            provider: "fake",
            state: "running",
            label: request.label ?? "",
          };
          byRequest.set(request.requestId, instance);
          started.push({ id: instance.id, cwd: request.cwd!, brief: inputText(request.input) });
          instances.set([...byRequest.values()]);
        }
        return instance;
      },
      capability<T>(id: string, key: { id: string }): T | undefined {
        if (key.id === terminate.id) return { terminate: async () => void stopped.push(id) } as T;
        if (key.id === messaging.id)
          return {
            submit: async (
              input: Parameters<typeof inputText>[0],
              options: { requestId: string },
            ) => {
              messages.push({ to: id, text: inputText(input) });
              return { id: options.requestId, delivery: "queued" };
            },
          } as T;
        return undefined;
      },
    }),
  });
  const runtime = agentRuntime({ id: "agents", programs: [fake] });
  // The test's own clock and agent activity: stalls are decided on them.
  let now = Date.now();
  const agents = {
    runtime: runtime.runtime,
    instances: runtime.instances,
    extensions: [runtime.extension],
    start: (agentsRuntime, request, signal) =>
      agentsRuntime.start(
        {
          profile: agentProfile({ program: fake, instructions: [...request.instructions] }),
          input: { content: [{ type: "text", data: request.brief }] },
          cwd: request.cwd,
          label: request.label,
          requestId: request.requestId,
        },
        signal,
      ),
  } as ManagedAgents;

  const issue: Issue = {
    id: "900",
    number: 900,
    title: "Make the thing durable",
    body: "**Goal:** durable.\n**Paths:** tooling/gate/thing.ts, docs/workflow.md\n",
    url: "",
    area: "tooling",
    stream: "s",
    group: "g",
    labels: ["task", "area:tooling", "kind:foundation"],
    state: "ready",
    assignees: [],
    owner: null,
    blockedBy: [],
    parent: null,
    progress: null,
  };
  const keyOf = <T>(id: string, cell: Cell<T>) => {
    const key = capability<Readable<T>>(id);
    return { key, extension: { id, provides: [provide(key, cell)] } };
  };
  const issues = keyOf("issues", new Cell<Issue[]>([issue]));
  const landingCell = new Cell<LandingStatus>();
  const landing = keyOf("landing", landingCell);
  const fullCheckCell = new Cell<FullCheckStatus>();
  const fullCheck = keyOf("full-check", fullCheckCell);
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const engine = openWorkflowSqlite({ path: join(directory, "engine.sqlite") });
  await writeFile(
    join(directory, "settings.json"),
    JSON.stringify({
      workerSlots: 2,
      maxRounds: 4,
      roles: Object.fromEntries(
        ["coordinator", "worker", "reviewer"].map((role) => [
          role,
          {
            pick: "first",
            models: [{ harness: "claude", model: "claude-opus-5-5", effort: "high" }],
          },
        ]),
      ),
    }),
  );
  const file = settingsFile({ path: join(directory, "settings.json") });
  const store = profiles({ state: state.state, settings: file.service });
  const tools = { id: "none", tools: [] };
  const route = taskRoute({
    root,
    issues: issues.key,
    landing: landing.key,
    fullCheck: fullCheck.key,
    profiles: store.service,
    assignments: () => work.assignments,
    agents,
    workerTools: tools,
    reviewerTools: tools,
    maxRounds: () => 4,
    clock: () => now,
  });
  const work = assignmentService({
    id: "work",
    state: state.state,
    engine: engine.engine,
    routes: [route],
  });
  const app = await startExtensions(
    defineConfig({
      id: "task-test",
      root,
      gate: { accept: () => false },
      extensions: [
        state.extension,
        engine.extension,
        issues.extension,
        landing.extension,
        fullCheck.extension,
        file.extension,
        store.extension,
        ...agents.extensions,
        work.extension,
      ],
    }),
  );
  t.after(() => app.stop());
  const service = app.context.get(work.service);
  const { id } = await service.submit(
    assignmentRequest(route, { key: "issue/900", input: { issue: 900, instructions: "" } }),
    { requestId: "submit" },
  );
  const read = (): Assignment => {
    const reading = service.assignments.get();
    if (reading.status !== "ready") throw new Error("not ready");
    return reading.value.find((assignment) => assignment.id === id)!;
  };
  const phase = async (expected: string, timeout = 20_000) => {
    const deadline = Date.now() + timeout;
    for (;;) {
      const assignment = read();
      if (assignment.error) throw new Error(`assignment faulted: ${assignment.error.message}`);
      if (taskState.parse(assignment.state).phase === expected)
        return taskState.parse(assignment.state);
      if (Date.now() > deadline)
        throw new Error(
          `still ${taskState.parse(assignment.state).phase} (${assignment.wait?.reason}), wanted ${expected}`,
        );
      await new Promise((resolve) => setTimeout(resolve, 50));
    }
  };
  const eventually = async (check: () => boolean, what: string) => {
    const deadline = Date.now() + 20_000;
    while (!check()) {
      if (Date.now() > deadline) throw new Error(`never ${what}: ${read().wait?.reason}`);
      await new Promise((resolve) => setTimeout(resolve, 50));
    }
  };
  const report = (data: object, requestId: string) =>
    service.report({ assignmentId: id, data: data as never, requestId });

  const designing = await phase("design");
  assert.equal(started.length, 1);
  assert.equal(started[0]!.cwd, join(directory, "tree-900"));
  assert.match(started[0]!.brief, /issue #900, Make the thing durable/);
  assert.deepEqual(designing.paths, ["docs/workflow.md", "tooling/gate/thing.ts"]);
  assert.deepEqual(
    read()
      .heldResources.map((claim) => claim.resource)
      .sort(),
    ["file:docs/workflow.md", "file:tooling/gate/thing.ts", "issue/900", "worker"],
  );

  await report({ kind: "design", summary: "Own it in one place." }, "design-1");
  await phase("approve");
  await report({ kind: "design_decision", accept: false, notes: "Name the owner." }, "decision-1");
  await phase("design");
  assert.match(messages.at(-1)!.text, /Design returned\. Name the owner\./);
  await report({ kind: "design", summary: "thing.ts owns it." }, "design-2");
  await phase("approve");
  await report({ kind: "design_decision", accept: true, notes: "" }, "decision-2");
  const building = await phase("build");
  assert.match(messages.at(-1)!.text, /^Design approved\./);

  // The worker's session ends mid-build: a fresh worker takes over with a handoff.
  const first = [...byRequest.entries()].find(
    ([, instance]) => instance.id === building.worker!.id,
  )!;
  byRequest.set(first[0], { ...first[1], state: "ended" });
  instances.set([...byRequest.values()]);
  const resumed = await (async () => {
    const deadline = Date.now() + 20_000;
    for (;;) {
      const state = taskState.parse(read().state);
      if (state.phase === "build" && state.worker!.id !== building.worker!.id) return state;
      if (Date.now() > deadline) throw new Error(`no replacement worker (${state.phase})`);
      await new Promise((resolve) => setTimeout(resolve, 50));
    }
  })();
  assert.equal(resumed.generation, 2);
  assert.match(started.at(-1)!.brief, /You replace a worker whose session ended/);
  assert.match(started.at(-1)!.brief, /The latest instruction it received:\nDesign approved\./);
  assert.match(started.at(-1)!.brief, /thing\.ts owns it\./);

  // An idle worker is nudged; its progress report holds nudges until its time;
  // after the nudges the coordinator is told; a coordinator message answers it.
  const worker = resumed.worker!.id;
  const until = async (
    check: (state: ReturnType<typeof taskState.parse>) => boolean,
    what: string,
  ) => {
    const deadline = Date.now() + 20_000;
    for (;;) {
      const state = taskState.parse(read().state);
      if (check(state)) return state;
      if (Date.now() > deadline) throw new Error(`never ${what}: ${JSON.stringify(state.watch)}`);
      await new Promise((resolve) => setTimeout(resolve, 50));
    }
  };
  // The worker's session reports its activity as its state. In production the
  // idle deadline wakes the route through its retryAt; this test's clock jumps
  // instead, so each observation also publishes a landing status with a new main
  // commit, a source the route watches and the build phase ignores.
  let tick = 0;
  const observe = (status: string) => {
    const [request, instance] = [...byRequest.entries()].find(
      ([, candidate]) => candidate.id === worker,
    )!;
    byRequest.set(request, { ...instance, state: status });
    instances.set([...byRequest.values()]);
    landingCell.set({
      service: { running: true, state: "idle", since: "", message: null },
      checking: null,
      waiting: [],
      held: [],
      day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null },
      recent: [],
      main: { commit: String(++tick).padStart(40, "0"), pushed: true },
    } as unknown as LandingStatus);
  };
  const nudges = () =>
    messages.filter(
      (message) => message.to === worker && /waits for your proved commit/.test(message.text),
    ).length;
  observe("idle");
  await until((state) => state.watch?.idleSince === now, "watched");
  now += STALL.idle;
  observe("idle");
  await until(() => nudges() === 1, "nudged");
  assert.match(messages.at(-1)!.text, /report_progress/);
  await report(
    { kind: "progress", role: "worker", note: "inner.ts runs", minutes: 30 },
    "progress-1",
  );
  await until((state) => state.progress?.note === "inner.ts runs", "progressed");
  now += 20 * 60_000;
  observe("idle");
  await new Promise((resolve) => setTimeout(resolve, 300));
  assert.equal(nudges(), 1, "idle within the reported time is expected");
  now += 10 * 60_000;
  observe("idle");
  await until(() => nudges() === 2, "nudged after the progress ran out");
  now += STALL.idle;
  observe("idle");
  await until(() => nudges() === 3, "nudged again");
  now += STALL.idle;
  observe("idle");
  const stalled = await until((state) => state.stalled !== null, "stalled");
  assert.match(
    stalled.stalled!,
    /stayed idle without your proved commit \(report_ready\) through 2 nudges; its last progress: inner\.ts runs/,
  );
  await report(
    { kind: "message", from: "coordinator", text: "Commit what you have." },
    "message-1",
  );
  await until(
    (state) =>
      state.stalled === null && state.note === "From the coordinator: Commit what you have.",
    "answered",
  );
  assert.equal(messages.at(-1)!.text, "From the coordinator: Commit what you have.");
  observe("busy");

  // A report naming a commit that is not the tree's clean HEAD faults loudly, never lands.
  await writeFile(join(building.tree!, "thing.txt"), "done");
  await git(building.tree!, "add", "thing.txt");
  await git(building.tree!, "commit", "-m", "the change");
  let commit = (await git(building.tree!, "rev-parse", "HEAD")).stdout.trim();
  await report({ kind: "ready", commit, summary: "focused, inner and supported green" }, "ready-1");
  const reviewing = await phase("reviewing");
  assert.equal(reviewing.reviewer!.commit, commit);
  assert.equal(started.length, 3);
  assert.match(started[2]!.brief, new RegExp(`review commit ${commit}`));

  await report(
    { kind: "review", commit, verdict: "changes", findings: "thing.txt:1 needs a newline" },
    "review-1",
  );
  await phase("build");
  assert.deepEqual(stopped, ["fake/2"]);
  assert.match(messages.at(-1)!.text, /needs a newline/);
  await report({ kind: "ready", commit, summary: "fixed" }, "ready-2");
  await phase("reviewing");
  // The second round's fresh reviewer gets the first round's findings to check first.
  assert.match(started.at(-1)!.brief, /This is review round 2\. Earlier rounds asked for:/);
  assert.match(started.at(-1)!.brief, /thing\.txt:1 needs a newline/);
  await report({ kind: "review", commit, verdict: "accept", findings: "" }, "review-2");
  await phase("landing");
  assert.deepEqual(JSON.parse(await readFile(join(directory, "queue.json"), "utf8")), [commit]);

  // A held landing goes back to build on the coordinator's rework: the held entry
  // is withdrawn, the worker is told why, and the same commit can queue again.
  await report({ kind: "rework", notes: "the tooling step hung in worker-memory" }, "rework-1");
  await phase("build");
  assert.match(
    messages.at(-1)!.text,
    /^Rework [0-9a-f]{40}: the tooling step hung in worker-memory/,
  );
  assert.deepEqual(JSON.parse(await readFile(join(directory, "queue.json"), "utf8")), []);
  // The worker shows the hold was not its change and reports the same commit: a
  // review accepted that exact commit, so it queues again without another review.
  await report({ kind: "ready", commit, summary: "the hang was the fixture's" }, "ready-3");
  await phase("landing");
  assert.deepEqual(JSON.parse(await readFile(join(directory, "queue.json"), "utf8")), [commit]);
  await report({ kind: "rework", notes: "it does touch the fixture" }, "rework-2");
  await phase("build");

  // A reviewer disputing the design escalates: the worker hears nothing until the
  // coordinator decides, and its message returns the task to build with the finding.
  await writeFile(join(building.tree!, "thing.txt"), "done, with the fixture fixed");
  // This round also changes the gate: once reviewed it waits for the coordinator.
  await mkdir(join(building.tree!, "tooling/gate"), { recursive: true });
  await writeFile(join(building.tree!, "tooling/gate/thing.ts"), "export {};\n");
  await git(building.tree!, "add", "tooling/gate/thing.ts");
  await git(building.tree!, "commit", "-am", "fix the fixture too");
  commit = (await git(building.tree!, "rev-parse", "HEAD")).stdout.trim();
  await report({ kind: "ready", commit, summary: "fixed the fixture" }, "ready-3b");
  await phase("reviewing");
  const told = messages.length;
  await report(
    {
      kind: "review",
      commit,
      verdict: "changes",
      findings: "the design owns the wrong file",
      disputesDesign: true,
    },
    "review-dispute",
  );
  const escalated = await phase("escalated");
  assert.match(
    escalated.escalation!,
    /disputes the approved design:\nthe design owns the wrong file/,
  );
  assert.equal(messages.length, told, "the worker is not sent to rework on the reviewer's word");
  await eventually(
    () =>
      /^The coordinator decides: The reviewer of [0-9a-f]{40} disputes/.test(
        read().wait?.reason ?? "",
      ),
    "waiting for the coordinator",
  );
  await report(
    { kind: "message", from: "coordinator", text: "Keep the design; answer the finding." },
    "decide-1",
  );
  const decided = await phase("build");
  assert.equal(decided.escalation, null);
  assert.match(
    messages.at(-1)!.text,
    /^From the coordinator: Keep the design; answer the finding\.\n\nIt decides on:\nThe review of [0-9a-f]{40} disputes/,
  );
  await report({ kind: "ready", commit, summary: "answered the finding" }, "ready-4");

  await phase("reviewing");
  await report({ kind: "review", commit, verdict: "accept", findings: "" }, "review-3");
  const signing = await phase("signoff");
  assert.deepEqual(signing.core, ["tooling/gate/thing.ts"]);
  assert.deepEqual(
    JSON.parse(await readFile(join(directory, "queue.json"), "utf8")),
    [],
    "not submitted before the sign-off",
  );
  await report({ kind: "signoff", accept: true, notes: "read it" }, "signoff-1");
  await phase("landing");
  assert.deepEqual(JSON.parse(await readFile(join(directory, "queue.json"), "utf8")), [commit]);

  const answer = (state: "landed" | "refused") =>
    ({
      service: { running: true, state: "idle", since: "", message: null },
      checking: null,
      waiting: [],
      held: [],
      day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null },
      recent: [
        {
          id: `${commit.slice(0, 12)}-1`,
          sha: commit,
          state,
          at: new Date(now).toISOString(),
          message: state,
          branch: "900",
          issue: 900,
          run: "/logs",
          wall_s: 1,
          estimate_s: 1,
          landing: [],
          deferred: [],
          pending: 0,
        },
      ],
      main: { commit, pushed: true },
    }) satisfies LandingStatus;
  landingCell.set(answer("landed"));
  const verifying = await phase("verify");
  assert.equal(verifying.landedMain, commit);
  // Landed: the worker stops, and waiting for verification holds no worker slot.
  assert.ok(stopped.includes(verifying.worker!.id), "the worker stops at landing");
  await eventually(
    () => read().heldResources.every((claim) => claim.resource === "issue/900"),
    "the worker slot and the card's paths released while verifying",
  );
  fullCheckCell.set({ verified: { commit, pushed: null } } as unknown as FullCheckStatus);
  const deadline = Date.now() + 20_000;
  while (read().status !== "finished" && Date.now() < deadline)
    await new Promise((resolve) => setTimeout(resolve, 50));
  assert.equal(read().status, "finished");
  assert.equal(taskState.parse(read().state).outcome, "verified");
  // fake/5 is the fresh reviewer after the dispute: its round counted, so its launch key is new.
  assert.deepEqual(stopped.sort(), ["fake/1", "fake/2", "fake/3", "fake/4", "fake/5"]);
  assert.deepEqual(read().heldResources, []);
});
