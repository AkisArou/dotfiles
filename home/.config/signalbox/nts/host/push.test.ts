import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { capability, Cell, defineConfig, provide, startExtensions } from "signalbox/core";
import type { Readable } from "signalbox/core";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { Issue, NeedsView, PushDevice, QuestionView } from "@signalbox/kit/shared/protocol";
import { fresh, notices, pushNotifications, pushService } from "./push.ts";
import type { PushPayload } from "./push.ts";

const question = (id: string, text: string, revision = 1): QuestionView => ({
  id,
  revision,
  status: "pending",
  question: text,
  context: "",
  issue: { number: 583, title: "Hardware", url: "https://example.invalid/583" },
  options: [],
  askedBy: "coordinator",
  requestedAt: 0,
  answer: null,
  then: null,
  answeredAt: null,
  posted: null,
});
const view = (questions: QuestionView[], unasked: number[] = []): NeedsView => ({
  questions,
  unasked: unasked.map((number) => ({ number, title: `Issue ${number}`, url: "" })),
  answered: [],
});

test("each waiting item is one notice, sent once while it waits and again if it comes back", () => {
  const now = notices(view([question("mac", "Buy a\n Mac  mini?")], [5]));
  assert.deepEqual(now, [
    { id: "question/mac/1", title: "Needs you · #583", body: "Buy a Mac mini?", url: "/needs-you" },
    {
      id: "issue/5",
      title: "Needs you · #5",
      body: "Issue 5: it waits on your word",
      url: "/needs-you",
    },
  ]);
  assert.deepEqual(fresh(now, undefined).send, [], "what waits at the first reading is not news");
  assert.deepEqual(fresh(now, ["question/mac/1"]).send, [now[1]]);
  assert.deepEqual(fresh(now, ["question/mac/1", "issue/5", "issue/9"]).sent, [
    "question/mac/1",
    "issue/5",
  ]);
  const askedAgain = notices(view([question("mac", "Buy a Mac mini?", 2)]));
  assert.equal(
    fresh(askedAgain, ["question/mac/1"]).send.length,
    1,
    "a question asked again is news",
  );
});

test("new needs reach every device once the issues are read; a forgotten device is dropped", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-push-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const needsKey = capability<Readable<NeedsView>>("needs");
  const issuesKey = capability<Readable<Issue[]>>("issues");
  const needs = new Cell<NeedsView>(view([question("mac", "Buy a Mac mini?")], [5]));
  const issues = new Cell<Issue[]>(); // not read yet
  const sent: { to: string; payload: PushPayload }[] = [];
  const forgotten = new Set<string>();
  const start = async () => {
    const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
    const app = await startExtensions(
      defineConfig({
        id: "push",
        root: directory,
        gate: { accept: () => false },
        extensions: [
          state.extension,
          { id: "inputs", provides: [provide(needsKey, needs), provide(issuesKey, issues)] },
          // Only the delivering extension: the page's source and operations need a host.
          pushNotifications({
            state: state.state,
            needs: needsKey,
            issues: issuesKey,
            directory,
            send: async (device, payload) => {
              if (forgotten.has(device.endpoint))
                throw Object.assign(new Error("gone"), { statusCode: 410 });
              sent.push({ to: device.endpoint, payload });
            },
          })[0]!,
        ],
      }),
    );
    return app;
  };
  const settle = () => new Promise((resolve) => setTimeout(resolve, 100));
  const device = (name: string): PushDevice => ({
    endpoint: `https://push.example.invalid/${name}`,
    keys: { p256dh: "key", auth: "auth" },
  });
  let app = await start();
  const push = app.context.get(pushService);
  await push.subscribe(device("phone"));
  await push.subscribe(device("laptop"));
  assert.equal(
    (await stat(join(directory, "push-keys.json"))).mode & 0o777,
    0o600,
    "the key pair is private",
  );

  await settle();
  issues.set([]);
  await settle();
  assert.equal(
    sent.length,
    0,
    "nothing until the issues are read, and then what already waits is not news",
  );

  needs.set(view([question("mac", "Buy a Mac mini?"), question("vps", "Rent a VPS?")], [5]));
  await settle();
  assert.deepEqual(
    sent.map((each) => [each.to.split("/").at(-1), each.payload.id, each.payload.count]).sort(),
    [
      ["laptop", "question/vps/1", 3],
      ["phone", "question/vps/1", 3],
    ],
  );

  sent.length = 0;
  forgotten.add(device("laptop").endpoint);
  needs.set(view([question("vps", "Rent a VPS?")], [5, 9]));
  await settle();
  assert.deepEqual(
    sent.map((each) => each.payload.id),
    ["issue/9"],
  );
  sent.length = 0;
  needs.set(view([question("vps", "Rent a VPS?")], [5, 9, 11]));
  await settle();
  assert.deepEqual(
    sent.map((each) => each.to.split("/").at(-1)),
    ["phone"],
    "the forgotten laptop is dropped",
  );

  sent.length = 0;
  await push.test(device("phone").endpoint);
  assert.equal(sent[0]?.payload.title, "Notifications are on");
  await assert.rejects(push.test(device("laptop").endpoint), /not subscribed/);

  // A restart sends nothing for what was already sent.
  sent.length = 0;
  await app.stop();
  app = await start();
  t.after(() => app.stop());
  await settle();
  assert.equal(sent.length, 0);
});
