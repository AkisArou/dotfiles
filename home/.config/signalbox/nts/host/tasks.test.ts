import { test } from "node:test";
import assert from "node:assert/strict";
import { projectIssues } from "./tasks.ts";
import type { ListedIssue } from "./tasks.ts";

const issue = (number: number, labels: string[], parent?: number): ListedIssue => ({
  number,
  title: `Issue ${number}`,
  body: "",
  url: `https://github.com/AkisArou/nts/issues/${number}`,
  labels: labels.map((name) => ({ name })),
  assignees: [],
  parent: parent ? { number: parent, title: `Issue ${parent}` } : null,
});
test("NTS issues retain hierarchy, scope, questions, blockers, owners and empty groups", () => {
  const listed = [
    issue(1, ["kind:parent"]),
    issue(2, ["kind:parent"]),
    { ...issue(3, ["kind:group"], 1), subIssuesSummary: { total: 10, completed: 8 } },
    issue(4, ["kind:group"], 2),
    {
      ...issue(5, ["task", "area:compiler", "in-progress", "agent:MainClaude"], 3),
      body: "blocked by: #7, #8",
    },
    issue(6, [], 1),
    issue(7, ["state:needs-user"], 2),
    issue(8, ["state:parked"]),
    issue(9, ["signalbox:state"]),
  ];
  const tasks = projectIssues(listed, "# Streams\n#2 then #1");
  assert.deepEqual(tasks.map((task) => task.number).slice(0, 2), [7, 4]);
  assert.ok(!tasks.some((task) => [1, 2, 3, 9].includes(task.number)));
  assert.equal(tasks.find((task) => task.number === 4)?.state, "group");
  const active = tasks.find((task) => task.number === 5)!;
  assert.equal(active.state, "in_progress");
  assert.equal(active.owner, "MainClaude");
  assert.equal(active.stream, "Issue 1");
  assert.equal(active.group, "Issue 3");
  assert.deepEqual(active.blockedBy, [7, 8]);
  assert.equal(tasks.find((task) => task.number === 6)?.state, "unscoped");
  assert.equal(tasks.find((task) => task.number === 8)?.state, "parked");
});

test("pool and landing claims project active work without changing held task policy", async () => {
  const { claimedIssues } = await import("@signalbox/kit/shared/claims");
  const tasks = projectIssues([issue(807, ["task"]), issue(808, ["state:held"])], "");
  const agent = {
    id: "native",
    name: "Codex worker",
    role: "codex",
    state: "external",
    path: "/pool/1/compiler",
    issue: null,
  };
  const projected = claimedIssues(
    tasks,
    [agent],
    [
      { issue: 807, what: "tree 1", path: "/pool/1", idleMs: 5 * 3_600_000 },
      { issue: 808, what: "held in landing" },
    ],
  );
  assert.equal(projected.find((task) => task.number === 807)?.state, "in_progress");
  assert.equal(projected.find((task) => task.number === 807)?.owner, "Codex worker");
  assert.equal(projected.find((task) => task.number === 807)?.workspace, "/pool/1");
  assert.equal(projected.find((task) => task.number === 808)?.state, "held");
  assert.equal(
    tasks.find((task) => task.number === 807)?.state,
    "ready",
    "observing work does not mutate the board",
  );
  // A tree nobody works in, untouched for a day, holds idle work; one used lately is in progress.
  const idle = (idleMs: number) =>
    claimedIssues(
      tasks,
      [],
      [{ issue: 807, what: "tree 6 · 10 unlanded", path: "/pool/6", idleMs }],
    ).find((task) => task.number === 807)?.state;
  assert.equal(idle(24 * 3_600_000), "orphaned");
  assert.equal(idle(10 * 60_000), "in_progress");
  assert.equal(
    claimedIssues(tasks, [], [{ issue: 807, what: "checking in the landing service" }]).find(
      (task) => task.number === 807,
    )?.state,
    "in_progress",
    "the landing service holding it is work in progress",
  );
});
