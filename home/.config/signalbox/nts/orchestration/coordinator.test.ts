// The coordinator wiring with stand-in agents: Signalbox launches generation 1
// with its brief, an event becomes one mailbox message, and the mailbox
// delivers it to the coordinator session exactly once.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { capability, Cell, defineConfig, provide, startExtensions } from "signalbox/core";
import type { Readable } from "signalbox/core";
import {
  agentProfile,
  agentProgram,
  agentRuntime,
  inputText,
  messaging,
} from "signalbox/ext/agents";
import type { AgentInstance } from "signalbox/ext/agents";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { Issue } from "@signalbox/kit/shared/protocol";
import type { ManagedAgents } from "./agents.ts";
import { ntsOrchestration } from "./index.ts";

async function until<T>(read: () => T | undefined, timeout = 30_000): Promise<T> {
  const deadline = Date.now() + timeout;
  for (;;) {
    const value = read();
    if (value !== undefined) return value;
    if (Date.now() > deadline) throw new Error("timed out");
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
}

test("the coordinator starts with its brief and receives each event once through its mailbox", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-coordinator-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const started: { id: string; brief: string; tools: string[] }[] = [];
  const delivered: { to: string; text: string; requestId: string }[] = [];
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
          started.push({
            id: instance.id,
            brief: inputText(request.input),
            tools: (request.tools ?? []).map((set) => set.id),
          });
          instances.set([...byRequest.values()]);
        }
        return instance;
      },
      capability<T>(id: string, key: { id: string }): T | undefined {
        if (key.id !== messaging.id) return undefined;
        return {
          submit: async (
            input: Parameters<typeof inputText>[0],
            options: { requestId: string },
          ) => {
            delivered.push({ to: id, text: inputText(input), requestId: options.requestId });
            return { id: options.requestId, delivery: "queued" };
          },
        } as T;
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
          tools: request.tools,
          requestId: request.requestId,
        },
        signal,
      ),
  } as ManagedAgents;
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
  const issuesKey = capability<Readable<Issue[]>>("issues");
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const orchestration = ntsOrchestration({
    root: directory,
    directory,
    state: state.state,
    issues: issuesKey,
    agents: () => agents,
    settings: { coordinator: true, file: join(directory, "settings.json") },
    // No issue is asked about here: GitHub is never written.
    gitHub: async () => {
      throw new Error("no GitHub in this test");
    },
    refreshIssues: async () => {},
  });
  const app = await startExtensions(
    defineConfig({
      id: "coordinator-test",
      root: directory,
      gate: { accept: () => false },
      extensions: [
        state.extension,
        { id: "issues", provides: [provide(issuesKey, new Cell<Issue[]>([]))] },
        ...orchestration.extensions,
      ],
    }),
  );
  t.after(() => app.stop());

  const coordinator = await until(() =>
    started.find((agent) => /coordinator generation 1/.test(agent.brief)),
  );
  assert.deepEqual(coordinator.tools, ["nts-coordinator"]);
  assert.match(coordinator.brief, /"checkpoint": null/);

  // The first observation only seeds the cursor; a later event is news.
  await new Promise((resolve) => setTimeout(resolve, 6_000));
  const decisions = app.context.get(orchestration.decisions);
  const asked = await decisions.request({
    id: "q1",
    input: { question: "Delete old pins?", context: "12 GB" },
  });
  await decisions.decide({
    id: "q1",
    expectedRevision: asked.revision,
    decision: { answer: "yes" },
    by: {},
    requestId: "d1",
  });
  const message = await until(
    () =>
      delivered.find((entry) =>
        /The owner answered q1: "Delete old pins\?"\nAnswer: yes/.test(entry.text),
      ),
    30_000,
  );
  assert.equal(message.to, coordinator.id);
  await new Promise((resolve) => setTimeout(resolve, 6_000));
  assert.equal(delivered.filter((entry) => /answered q1/.test(entry.text)).length, 1);
});
