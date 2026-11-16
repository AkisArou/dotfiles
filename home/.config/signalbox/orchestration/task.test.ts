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
import { agentProfile, agentProgram, agentRuntime, inputText } from "signalbox/ext/agents/index";
import type { AgentInstance } from "signalbox/ext/agents/index";
import { messaging, terminate } from "signalbox/ext/agents/features";
import { assignmentRequest, assignmentService } from "signalbox/ext/assignments/index";
import type { Assignment } from "signalbox/ext/assignments/index";
import { openWorkflowSqlite } from "signalbox/ext/integrations/openworkflow";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { Issue } from "../shared/model.ts";
import type { LandingStatus } from "../nts/landing.ts";
import type { FullCheckStatus } from "../nts/full-check.ts";
import type { ManagedAgents } from "./agents.ts";
import { profiles } from "./profiles.ts";
import { taskRoute, taskState } from "./task.ts";

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
if (command === "status") console.log(JSON.stringify({ pool: root, room: { ok: true, free_bytes: 1, floor_bytes: 0, unbuilt: 0, why: null }, trees: trees() }));
else if (command === "take") { const n = rest[1]; if (!existsSync(tree(n))) execFileSync("git", ["worktree", "add", "-b", n, tree(n), "main"]); }
`;
// check.ts: `<sha> --no-wait` records the submission; `status --json` shows it waiting.
const CHECK = `import { readFileSync, writeFileSync, existsSync } from "node:fs";
const file = process.env.STAND_IN_QUEUE, [first] = process.argv.slice(2);
const queue = existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : [];
if (first === "status") console.log(JSON.stringify({ service: { running: true, state: "idle", since: "", message: null }, checking: null,
  waiting: queue.map((sha) => ({ id: sha.slice(0, 12), sha, branch: null, issue: null, by: "t", submitted_at: "", note: null })),
  held: [], day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null }, recent: [], main: { commit: null, pushed: null } }));
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
          instance = { id: `fake/${byRequest.size}`, provider: "fake", state: "running", label: request.label ?? "" };
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
            submit: async (input: Parameters<typeof inputText>[0], options: { requestId: string }) => {
              messages.push({ to: id, text: inputText(input) });
              return { id: options.requestId, delivery: "queued" };
            },
          } as T;
        return undefined;
      },
    }),
  });
  const runtime = agentRuntime({ id: "agents", programs: [fake] });
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
  const keyOf = <T,>(id: string, cell: Cell<T>) => {
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
  const store = profiles({
    state: state.state,
    initial: {
      profiles: [{ id: "p", label: "p", harness: "claude", model: "m", effort: "high" }],
      roles: { coordinator: { profile: "p" }, worker: { profile: "p" }, reviewer: { profile: "p" } },
    },
  });
  const tools = { id: "none", tools: [] };
  const route = taskRoute({
    root,
    issues: issues.key,
    landing: landing.key,
    fullCheck: fullCheck.key,
    profiles: store.service,
    agents,
    workerTools: tools,
    reviewerTools: tools,
    maxRounds: 4,
  });
  const work = assignmentService({ id: "work", state: state.state, engine: engine.engine, routes: [route] });
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
      if (taskState.parse(assignment.state).phase === expected) return taskState.parse(assignment.state);
      if (Date.now() > deadline)
        throw new Error(`still ${taskState.parse(assignment.state).phase} (${assignment.wait?.reason}), wanted ${expected}`);
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
    read().heldResources.map((claim) => claim.resource).sort(),
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

  // A report naming a commit that is not the tree's clean HEAD faults loudly, never lands.
  await writeFile(join(building.tree!, "thing.txt"), "done");
  await git(building.tree!, "add", "thing.txt");
  await git(building.tree!, "commit", "-m", "the change");
  const commit = (await git(building.tree!, "rev-parse", "HEAD")).stdout.trim();
  await report({ kind: "ready", commit, summary: "focused, inner and supported green" }, "ready-1");
  const reviewing = await phase("reviewing");
  assert.equal(reviewing.reviewer!.commit, commit);
  assert.equal(started.length, 2);
  assert.match(started[1]!.brief, new RegExp(`review commit ${commit}`));

  await report({ kind: "review", commit, verdict: "changes", findings: "thing.txt:1 needs a newline" }, "review-1");
  await phase("build");
  assert.deepEqual(stopped, ["fake/1"]);
  assert.match(messages.at(-1)!.text, /needs a newline/);
  await report({ kind: "ready", commit, summary: "fixed" }, "ready-2");
  await phase("reviewing");
  await report({ kind: "review", commit, verdict: "accept", findings: "" }, "review-2");
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
        { id: `${commit.slice(0, 12)}-1`, sha: commit, state, at: "", message: state, branch: "900", issue: 900, run: "/logs",
          wall_s: 1, estimate_s: 1, landing: [], deferred: [], pending: 0 },
      ],
      main: { commit, pushed: true },
    }) satisfies LandingStatus;
  landingCell.set(answer("landed"));
  const verifying = await phase("verify");
  assert.equal(verifying.landedMain, commit);
  fullCheckCell.set({ verified: { commit, pushed: null } } as unknown as FullCheckStatus);
  const deadline = Date.now() + 20_000;
  while (read().status !== "finished" && Date.now() < deadline) await new Promise((resolve) => setTimeout(resolve, 50));
  assert.equal(read().status, "finished");
  assert.equal(taskState.parse(read().state).outcome, "verified");
  assert.deepEqual(stopped.sort(), ["fake/0", "fake/1", "fake/2"]);
  assert.deepEqual(read().heldResources, []);
});
