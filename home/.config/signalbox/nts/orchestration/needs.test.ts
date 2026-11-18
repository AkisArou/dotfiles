// The GitHub tracker on the kit's Needs you: asking labels the issue
// state:needs-user, answering comments and relabels it, and a failed write is
// retried, never repeated once done.
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
import {
  answerFor,
  needs,
  questionAnswer,
  questionInput,
} from "@signalbox/kit/orchestration/needs";
import { gitHubTracker } from "./github.ts";

const issue = (number: number, state: Issue["state"]): Issue =>
  ({
    id: String(number),
    number,
    title: `Issue ${number}`,
    url: `https://github.com/o/r/issues/${number}`,
    state,
    labels: state === "needs-user" ? ["task", "state:needs-user"] : ["task"],
  }) as Issue;

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
    project: "nts",
    state: state.state,
    questions: questions.service,
    issues: issuesKey,
    tracker: gitHubTracker(gitHub),
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
