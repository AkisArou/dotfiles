// The NTS project's own verbs for sbn: read and steer tasks by issue number
// through the host's operations, as the browser and TUI do.
//   sbn status                      active tasks: phase, time in it, what each waits for
//   sbn show <issue>                one task's design, summary and latest instruction
//   sbn assign <issue> [notes...]   start a task
//   sbn approve <issue> [notes...]  approve the design
//   sbn return <issue> <notes...>   send the design back
//   sbn message <issue> <text...>   message the worker (answers help, decides an escalation)
//   sbn rework <issue> <notes...>   send a held or wrong landing back to build
//   sbn retry <issue>               retry a faulted task
//   sbn cancel <issue> <reason...>  cancel a task (its tree and branch stay with the pool)
import { readFileSync } from "node:fs";
import { randomUUID } from "node:crypto";
import { connectClient } from "signalbox/host";
import type { HostClient } from "signalbox/host";
import type { AssignmentView } from "../shared/model.ts";
import { assignmentPhase, assignmentSays } from "../shared/model.ts";

const project = JSON.parse(readFileSync(new URL("../project.json", import.meta.url), "utf8")) as {
  apiPort: number;
};
const url = process.env.SIGNALBOX_API_URL ?? `ws://127.0.0.1:${project.apiPort}/rpc`;

const VERBS = ["status", "show", "assign", "approve", "return", "message", "rework", "retry", "cancel"] as const;
type Verb = (typeof VERBS)[number];

async function assignments(client: HostClient): Promise<AssignmentView[]> {
  const reading = await client.request<{ status: string; value?: AssignmentView[] }>({
    type: "read",
    ref: "nts.assignments",
  });
  if (reading.status !== "ready" || !reading.value) throw new Error(`Assignments are ${reading.status}`);
  return reading.value;
}

/** The issue's live assignment: active or queued, else its latest. */
async function task(client: HostClient, issue: number): Promise<AssignmentView> {
  const all = (await assignments(client)).filter((a) => a.issue === issue);
  const found =
    all.find((a) => a.status === "active" || a.status === "queued") ??
    all.sort((a, b) => b.acceptedAt - a.acceptedAt)[0];
  if (!found) throw new Error(`No task for #${issue}`);
  return found;
}

const call = (client: HostClient, ref: string, input: object) =>
  client.request({ type: "call", ref, input, requestId: `cli/${randomUUID()}` });

async function main(verb: Verb, args: string[]): Promise<void> {
  const client = await connectClient({ url });
  try {
    if (verb === "status") {
      const live = (await assignments(client)).filter((a) => a.status === "active" || a.status === "queued");
      if (!live.length) return console.log("No active tasks.");
      for (const a of live.sort((x, y) => (x.issue ?? 0) - (y.issue ?? 0)))
        console.log(`#${a.issue ?? "?"}\t${assignmentPhase(a)}\t${assignmentSays(a).split("\n", 1)[0]}`);
      return;
    }
    const issue = Number(args.shift()?.replace(/^#/, ""));
    if (!Number.isInteger(issue) || issue <= 0) throw new Error(`${verb} needs an issue number`);
    const text = args.join(" ").trim();
    const needsText = (what: string) => {
      if (!text) throw new Error(`${verb} needs ${what}`);
      return text;
    };
    if (verb === "assign")
      return void console.log(await call(client, "nts.tasks.assign", { issue, instructions: text }));
    const a = await task(client, issue);
    const assignment = a.id;
    switch (verb) {
      case "show":
        return console.log(
          [
            `#${issue} ${a.status} ${assignmentPhase(a)} (${a.id})`,
            assignmentSays(a),
            a.design && `\nDesign:\n${a.design}`,
            a.summary && `\nLatest report:\n${a.summary}`,
            a.commit && `\nCommit ${a.commit}${a.tree ? ` in ${a.tree}` : ""}`,
          ]
            .filter(Boolean)
            .join("\n"),
        );
      case "approve":
        await call(client, "nts.assignments.decide-design", { assignment, accept: true, notes: text });
        break;
      case "return":
        await call(client, "nts.assignments.decide-design", { assignment, accept: false, notes: needsText("notes") });
        break;
      case "message":
        await call(client, "nts.assignments.message-worker", { assignment, text: needsText("a message") });
        break;
      case "rework":
        await call(client, "nts.assignments.rework", { assignment, notes: needsText("notes") });
        break;
      case "retry":
        await call(client, "nts.assignments.retry", { assignment });
        break;
      case "cancel":
        await call(client, "nts.assignments.cancel", { assignment, reason: needsText("a reason") });
        break;
    }
    console.log(`#${issue}: ${verb} sent`);
  } finally {
    client.close();
  }
}

const [verb, ...args] = process.argv.slice(2);
if (!verb || !(VERBS as readonly string[]).includes(verb)) {
  console.error(`Usage: sbn <${VERBS.join("|")}> [issue] [text...]`);
  process.exitCode = 2;
} else
  await main(verb as Verb, args).catch((error: unknown) => {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  });
