// Needs you: every entry is a question or an issue waiting for one; asking about
// an issue labels it, answering posts on it and moves it where the answer says,
// and a failed GitHub write is reported and tried again, never lost.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  capability,
  Cell,
  defineConfig,
  latestValue,
  provide,
  startExtensions,
} from "signalbox/core";
import type { Readable } from "signalbox/core";
import { decisionRequests } from "signalbox/ext/decisions";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { Issue, NeedsView } from "@signalbox/kit/shared/protocol";
import { answerFor, needs, needsView, questionAnswer, questionInput } from "./needs.ts";

const issue = (number: number, state: Issue["state"]): Issue =>
  ({
    id: String(number),
    number,
    title: `Issue ${number}`,
    url: `https://github.com/o/r/issues/${number}`,
    state,
    labels: state === "needs-user" ? ["task", "state:needs-user"] : ["task"],
  }) as Issue;
const request = (id: string, status: "pending" | "decided", input: object, decided?: object) =>
  ({
    id,
    input: { question: `Q ${id}`, context: "", ...input },
    status,
    revision: 1,
    requestedAt: 1,
    expiresAt: null,
    result: decided ? { decision: decided, by: {}, time: 50 } : null,
  }) as never;

test("an issue waits for a question unless one is open about it or its answer already reached GitHub", () => {
  const issues = [
    issue(1, "needs-user"),
    issue(2, "needs-user"),
    issue(3, "needs-user"),
    issue(4, "ready"),
  ];
  const view = needsView(
    [
      request("asked", "pending", { issue: 1 }),
      // Answered and posted after the issues were read (at 100): its label is already gone.
      request("posted", "decided", { issue: 2 }, { answer: "go", then: "ready" }),
    ],
    issues,
    100,
    { posted: { posted: 150 } },
  );
  assert.deepEqual(
    view.questions.map((q) => q.id),
    ["asked"],
  );
  assert.deepEqual(
    view.unasked.map((i) => i.number),
    [3],
    "only #3 waits with nobody asking",
  );
  assert.equal(view.answered[0]!.posted, "done");
  assert.equal(view.questions[0]!.issue?.title, "Issue 1");
  // Once the issues are read again with #2 still labelled, it waits for a question again.
  assert.deepEqual(
    needsView(
      [request("posted", "decided", { issue: 2 }, { answer: "go" })],
      [issue(2, "needs-user")],
      200,
      {
        posted: { posted: 150 },
      },
    ).unasked.map((i) => i.number),
    [2],
  );
});

test("an option answers with its label and state; written text takes the chosen state", () => {
  const input = questionInput.parse({
    question: "Buy a Mac mini?",
    context: "",
    issue: 583,
    options: [{ label: "No, leave it unrun", then: "parked" }, { label: "Yes" }],
  });
  assert.deepEqual(answerFor(input, { option: 0 }), {
    answer: "No, leave it unrun",
    option: 0,
    then: "parked",
  });
  assert.deepEqual(answerFor(input, { option: 1 }), { answer: "Yes", option: 1, then: "ready" });
  assert.deepEqual(answerFor(input, { answer: " Only for CI ", then: "held" }), {
    answer: "Only for CI",
    then: "held",
  });
  assert.throws(() => answerFor(input, { option: 5 }), /no option 6/);
  assert.deepEqual(answerFor({ question: "q", context: "" }, { answer: "fine" }), {
    answer: "fine",
  });
});

test("asking labels the issue; answering posts, relabels, and a failed write is retried", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-needs-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const labels = new Map<number, string[]>([[583, ["task"]]]);
  const calls: string[][] = [];
  let failNextComment = true;
  const gitHub = async (args: readonly string[]) => {
    calls.push([...args]);
    const number = Number(args[2]);
    if (args[0] === "issue" && args[1] === "view")
      return JSON.stringify({ labels: (labels.get(number) ?? []).map((name) => ({ name })) });
    if (args[1] === "comment" && failNextComment) {
      failNextComment = false;
      throw new Error("HTTP 502");
    }
    if (args[1] === "edit") {
      const have = labels.get(number) ?? [];
      for (let i = 3; i < args.length; i += 2)
        if (args[i] === "--add-label") have.push(args[i + 1]!);
        else if (args[i] === "--remove-label") have.splice(have.indexOf(args[i + 1]!), 1);
      labels.set(number, have);
    }
    return "";
  };
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const questions = decisionRequests({
    id: "q",
    state: state.state,
    input: questionInput,
    decision: questionAnswer,
  });
  const issuesKey = capability<Readable<Issue[]>>("issues");
  let refreshed = 0;
  const waiting = needs({
    state: state.state,
    questions: questions.service,
    issues: issuesKey,
    gitHub,
    refreshIssues: async () => void refreshed++,
    retryMs: 300,
  });
  const app = await startExtensions(
    defineConfig({
      id: "needs",
      root: directory,
      gate: { accept: () => false },
      extensions: [
        state.extension,
        questions.extension,
        { id: "issues", provides: [provide(issuesKey, new Cell<Issue[]>([issue(583, "ready")]))] },
        waiting.extension,
      ],
    }),
  );
  t.after(() => app.stop());
  const service = app.context.get(questions.service);
  const view = () => latestValue(app.context.get(waiting.view)) as NeedsView;
  const until = async (done: () => boolean, what: string) => {
    for (let i = 0; i < 100 && !done(); i++) await new Promise((r) => setTimeout(r, 50));
    assert.ok(done(), what);
  };

  const asked = await service.request({
    id: "mac",
    input: {
      question: "Buy a Mac mini?",
      context: "",
      issue: 583,
      options: [{ label: "No", then: "parked" }],
    },
  });
  await until(() => labels.get(583)!.includes("state:needs-user"), "asking labels the issue");

  await service.decide({
    id: "mac",
    expectedRevision: asked.revision,
    decision: answerFor(asked.input, { option: 0 }),
    by: { owner: "local" },
    requestId: "answer",
  });
  await until(() => view().answered[0]?.posted === "HTTP 502", "a failed comment is reported");
  assert.ok(
    labels.get(583)!.includes("state:needs-user"),
    "nothing is relabelled before the comment posts",
  );

  // The next pass after the retry delay tries again, and it posts.
  await until(() => view().answered[0]?.posted === "done", "the retry posts the answer");
  assert.deepEqual(labels.get(583), ["task", "state:parked"], "needs-user goes, parked comes");
  const comment = calls.find((call) => call[1] === "comment" && call.includes("--body"))!;
  assert.match(
    comment[comment.indexOf("--body") + 1]!,
    /Decided by the owner[\s\S]*Buy a Mac mini\?[\s\S]*No[\s\S]*parked/,
  );
  assert.ok(refreshed > 0, "the issues are read again after the label change");
  assert.equal(
    calls.filter((call) => call[1] === "comment").length,
    2,
    "one failure, one post, no repeat",
  );
});
