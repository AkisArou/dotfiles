// The NTS project's own verbs for sbn: read and steer tasks by issue number
// through the host's operations, as the browser and TUI do.
//   sbn status                      how it runs (workers, rounds, pause), then active tasks
//   sbn settings                    settings.json's values, the pause, harnesses
//   sbn workers <n>                 tasks holding a worker at once (written to settings.json)
//   sbn rounds <n>                  returns to build before a task escalates (settings.json)
//   sbn share <group> <percent>     a group's share of the worker slots (the others take the rest)
//   sbn role <role> <model...>      a role's models in order (ids from sbn models)
//   sbn pick <role> <first|spread|coordinator>
//                                   how a launch chooses among the role's models
//   sbn models                      every model and effort the harnesses offer, by id
//   sbn pause <reason...>           admit no new task; admitted tasks carry on
//   sbn resume                      admit tasks again
//   sbn needs                       what waits on you: questions (with their ids), issues not asked yet
//   sbn answer <id> <text...> [--then ready|parked|held]
//   sbn answer <id> --option <n>    answer a question (its choice n, or your words)
//   sbn answer #<issue> <text...> [--then ready|parked|held]
//                                   answer an issue that waits on you without a question
//   sbn withdraw <id>               withdraw an open question (the coordinator is told)
//   sbn show <issue>                one task's design, summary and latest instruction
//   sbn assign <issue> [--model <id>] [notes...]
//                                   start a task (a model when workers pick "coordinator")
//   sbn approve <issue> [notes...]  approve the design
//   sbn return <issue> <notes...>   send the design back
//   sbn signoff <issue> [notes...]  sign off a reviewed core change (queues it)
//   sbn refuse <issue> <notes...>   refuse it: back to build with the notes
//   sbn message <issue> [--as coordinator] <text...>
//                                   message the worker (answers help, decides an escalation);
//                                   --as coordinator when the owner stands in for the coordinator
//   sbn rework <issue> <notes...>   send a held or wrong landing back to build
//   sbn prioritize <issue> <-9..9>  move a task ahead (higher first; 0 is the default)
//   sbn retry <issue>               retry a faulted task
//   sbn coordinator <reason...>     a fresh coordinator generation from its checkpoint
//   sbn end-agent <name|id>         end one managed agent session (a stray or replaced one)
//   sbn cancel <issue> <reason...>  cancel a task (its tree and branch stay with the pool)
import { readFileSync } from "node:fs";
import { randomUUID } from "node:crypto";
import { connectClient } from "signalbox/host";
import type { HostClient } from "signalbox/host";
import type { AssignmentView } from "../shared/model.ts";
import type { NeedsView, SettingsView } from "../shared/model.ts";
import {
  assignmentPhase,
  assignmentSays,
  issueStateLabels,
  modelsLine,
  needsCount,
  pickLabels,
  settingsLine,
} from "../shared/model.ts";

const project = JSON.parse(readFileSync(new URL("../project.json", import.meta.url), "utf8")) as {
  apiPort: number;
};
const url = process.env.SIGNALBOX_API_URL ?? `ws://127.0.0.1:${project.apiPort}/rpc`;

const VERBS = [
  "status",
  "settings",
  "workers",
  "rounds",
  "pause",
  "resume",
  "role",
  "share",
  "pick",
  "models",
  "needs",
  "answer",
  "withdraw",
  "show",
  "assign",
  "prioritize",
  "approve",
  "return",
  "signoff",
  "refuse",
  "message",
  "rework",
  "retry",
  "cancel",
  "coordinator",
  "end-agent",
] as const;
type Verb = (typeof VERBS)[number];

async function read<T>(client: HostClient, ref: string): Promise<T> {
  const reading = await client.request<{ status: string; value?: T }>({ type: "read", ref });
  if (reading.status !== "ready" || reading.value === undefined)
    throw new Error(`${ref} is ${reading.status}`);
  return reading.value;
}

async function assignments(client: HostClient): Promise<AssignmentView[]> {
  const reading = await client.request<{ status: string; value?: AssignmentView[] }>({
    type: "read",
    ref: "nts.assignments",
  });
  if (reading.status !== "ready" || !reading.value)
    throw new Error(`Assignments are ${reading.status}`);
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
    const count = (what: string) => {
      const value = Number(args[0]);
      if (args.length !== 1 || !Number.isInteger(value)) throw new Error(`${verb} needs ${what}`);
      return value;
    };
    const settings = () => read<SettingsView>(client, "nts.settings");
    const change = async (input: object) => {
      await call(client, "nts.settings.set", { ...input, version: (await settings()).version });
      return console.log(settingsLine(await settings()));
    };
    if (verb === "workers" || verb === "rounds")
      return await change({
        [verb === "workers" ? "workerSlots" : "maxRounds"]: count("a whole number"),
      });
    if (verb === "share") {
      const [name, percent, ...rest] = args;
      if (!name || percent === undefined || rest.length)
        throw new Error("share needs <group> <percent 0-100>");
      return await change({ share: { name, percent: Number(percent.replace(/%$/, "")) } });
    }
    if (verb === "role") {
      const [role, ...models] = args;
      if (!role || !models.length)
        throw new Error("role needs <coordinator|worker|reviewer> <model id...>");
      return await change({ roles: { [role]: { models } } });
    }
    if (verb === "pick") {
      const [role, pick, ...rest] = args;
      if (!role || !pick || rest.length)
        throw new Error("pick needs <coordinator|worker|reviewer> <first|spread|coordinator>");
      return await change({ roles: { [role]: { pick } } });
    }
    if (verb === "pause") {
      const reason = args.join(" ").trim();
      if (!reason) throw new Error("pause needs a reason");
      await call(client, "nts.tasks.pause", { reason });
      return console.log(settingsLine(await settings()));
    }
    if (verb === "resume") {
      await call(client, "nts.tasks.resume", {});
      return console.log(settingsLine(await settings()));
    }
    if (verb === "models") {
      for (const model of (await settings()).catalog) console.log(`${model.id}\t${model.label}`);
      return;
    }
    if (verb === "needs") {
      const now = await read<NeedsView>(client, "nts.needs");
      if (!needsCount(now)) console.log("Nothing waits on you.");
      for (const question of now.questions) {
        console.log(
          `? ${question.id}${question.issue ? ` (#${question.issue.number} ${question.issue.title})` : ""}`,
        );
        console.log(`  ${question.question}`);
        if (question.context) console.log(question.context.replace(/^/gm, "    "));
        question.options.forEach((option, index) =>
          console.log(
            `  ${index + 1}. ${option.label}${question.issue ? ` → ${issueStateLabels[option.then ?? "ready"]}` : ""}`,
          ),
        );
      }
      for (const issue of now.unasked)
        console.log(`? #${issue.number} ${issue.title} (waits on you; not asked yet)`);
      for (const question of now.answered.slice(0, 3))
        console.log(
          `✓ ${question.id}: ${question.answer}${question.issue ? ` · #${question.issue.number} ${question.posted === "done" ? "posted" : `not posted yet: ${question.posted}`}` : ""}`,
        );
      return;
    }
    if (verb === "withdraw") {
      const id = args.join(" ").trim();
      const question = (await read<NeedsView>(client, "nts.needs")).questions.find(
        (candidate) => candidate.id === id,
      );
      if (!question) throw new Error(`${id} is not an open question (sbn needs lists them)`);
      await call(client, "nts.needs.withdraw", { id, revision: question.revision });
      return console.log(`${id}: withdrawn; the coordinator is told.`);
    }
    if (verb === "answer") {
      const flag = (name: string) => {
        const at = args.indexOf(name);
        return at < 0 ? undefined : args.splice(at, 2)[1];
      };
      const then = flag("--then");
      const choice = flag("--option");
      const [target, ...words] = args;
      const text = words.join(" ").trim();
      if (!target || (!text && choice === undefined))
        throw new Error("answer needs <id|#issue> and your words, or --option <n>");
      if (then !== undefined && !["ready", "parked", "held"].includes(then))
        throw new Error("--then takes ready, parked or held");
      if (target.startsWith("#")) {
        await call(client, "nts.needs.answer-issue", {
          issue: Number(target.slice(1)),
          answer: text,
          then: then ?? "ready",
        });
        return console.log(
          `${target}: answered; it goes on the issue, and the coordinator is told.`,
        );
      }
      const question = (await read<NeedsView>(client, "nts.needs")).questions.find(
        (candidate) => candidate.id === target,
      );
      if (!question) throw new Error(`${target} is not an open question (sbn needs lists them)`);
      await call(
        client,
        "nts.needs.answer",
        choice !== undefined
          ? { id: question.id, revision: question.revision, option: Number(choice) - 1 }
          : {
              id: question.id,
              revision: question.revision,
              answer: text,
              ...(then ? { then } : {}),
            },
      );
      return console.log(`${target}: answered; the coordinator is told.`);
    }
    if (verb === "settings") {
      const now = await settings();
      console.log(`${now.path} (version ${now.version})`);
      if (now.error) console.log(`ERROR, not in force (the last good settings are):\n${now.error}`);
      console.log(settingsLine(now));
      for (const [role, choice] of Object.entries(now.roles)) {
        console.log(`${role}: picks ${pickLabels[choice.pick]}`);
        for (const model of choice.models) console.log(`  ${model.id}\t${model.label}`);
      }
      for (const [harness, blocked] of Object.entries(now.unavailable))
        if (blocked) console.log(`${harness}: unavailable (${blocked.reason})`);
      return;
    }
    if (verb === "status") {
      const now = await settings();
      console.log(`${settingsLine(now)} · workers use ${modelsLine(now.roles.worker)}`);
      const live = (await assignments(client)).filter(
        (a) => a.status === "active" || a.status === "queued",
      );
      if (!live.length) return console.log("No active tasks.");
      for (const a of live.sort((x, y) => (x.issue ?? 0) - (y.issue ?? 0)))
        console.log(
          `#${a.issue ?? "?"}\t${assignmentPhase(a)}\t${assignmentSays(a).split("\n", 1)[0]}`,
        );
      return;
    }
    if (verb === "end-agent") {
      const wanted = args.join(" ").trim();
      if (!wanted) throw new Error("end-agent needs an agent name or id");
      const listed = await client.request<{
        status: string;
        value?: { id: string; name: string; state: string }[];
      }>({ type: "read", ref: "nts.agents" });
      const matches = (listed.value ?? []).filter(
        (agent) => agent.state !== "ended" && (agent.id === wanted || agent.name === wanted),
      );
      if (matches.length !== 1)
        throw new Error(
          `${matches.length} live agents match "${wanted}"; use the exact name or id`,
        );
      await call(client, "nts.agents.end", { id: matches[0]!.id });
      return console.log(`ended ${matches[0]!.name} (${matches[0]!.id})`);
    }
    if (verb === "coordinator") {
      const reason = args.join(" ").trim();
      if (!reason) throw new Error("coordinator needs a reason");
      await call(client, "nts.coordinator.replace", { reason });
      return console.log("coordinator: replacement requested (checkpoint, then a new generation)");
    }
    const issue = Number(args.shift()?.replace(/^#/, ""));
    const modelAt = args.indexOf("--model");
    const model = modelAt >= 0 ? args.splice(modelAt, 2)[1] : undefined;
    if (modelAt >= 0 && !model) throw new Error("--model needs a model id (sbn models lists them)");
    const asAt = args.indexOf("--as");
    const as = asAt >= 0 ? args.splice(asAt, 2)[1] : undefined;
    if (as !== undefined && as !== "user" && as !== "coordinator")
      throw new Error("--as takes user or coordinator");
    if (!Number.isInteger(issue) || issue <= 0) throw new Error(`${verb} needs an issue number`);
    const text = args.join(" ").trim();
    const needsText = (what: string) => {
      if (!text) throw new Error(`${verb} needs ${what}`);
      return text;
    };
    if (verb === "prioritize") {
      const urgency = Number(needsText("an urgency from -9 to 9"));
      const { urgency: all } = (await call(client, "nts.tasks.prioritize", { issue, urgency })) as {
        urgency: Record<string, number>;
      };
      return console.log(`#${issue}: urgency ${urgency}; set: ${JSON.stringify(all)}`);
    }
    if (verb === "assign") {
      const { id } = (await call(client, "nts.tasks.assign", {
        issue,
        instructions: text,
        ...(model ? { model } : {}),
      })) as {
        id: string;
      };
      return console.log(`#${issue}: assigned (${id})`);
    }
    const a = await task(client, issue);
    const assignment = a.id;
    switch (verb) {
      case "show":
        return console.log(
          [
            `#${issue} ${a.status} ${assignmentPhase(a)} (${a.id})`,
            assignmentSays(a),
            a.design && `\nDesign:\n${a.design}`,
            a.note && `\nLatest instruction to the worker:\n${a.note}`,
            a.summary && `\nLatest report:\n${a.summary}`,
            a.commit && `\nCommit ${a.commit}${a.tree ? ` in ${a.tree}` : ""}`,
          ]
            .filter(Boolean)
            .join("\n"),
        );
      case "approve":
        await call(client, "nts.assignments.decide-design", {
          assignment,
          accept: true,
          notes: text,
        });
        break;
      case "return":
        await call(client, "nts.assignments.decide-design", {
          assignment,
          accept: false,
          notes: needsText("notes"),
        });
        break;
      case "signoff":
        await call(client, "nts.assignments.sign-off", { assignment, accept: true, notes: text });
        break;
      case "refuse":
        await call(client, "nts.assignments.sign-off", {
          assignment,
          accept: false,
          notes: needsText("notes"),
        });
        break;
      case "message":
        await call(client, "nts.assignments.message-worker", {
          assignment,
          text: needsText("a message"),
          from: as ?? "user",
        });
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
