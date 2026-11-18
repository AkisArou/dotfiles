// What waits on the owner, in one list. Every entry is a question in one
// sentence. The coordinator asks with ask_user: about an issue when it is one,
// with choices when the answer is one, each saying where the issue goes after it
// (ready, parked or held). An issue labelled state:needs-user that nobody has
// asked about is listed as unasked, and the coordinator is told to state its
// question; the owner can also answer it directly.
//
// Asking about an issue labels it state:needs-user, so its task stays blocked.
// Answering decides the question, posts the answer on the issue, moves the issue
// to the state the answer names, and wakes the coordinator. The GitHub writes are
// effects recorded once done and retried until they are.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { z } from "zod";
import { capability, Cell, combineReadables, latestValue, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import type { WorkflowStore } from "signalbox/ext/assignments";
import type { DecisionRequest, Decisions } from "signalbox/ext/decisions";
import type { Issue, IssueState, NeedsView, QuestionView } from "@signalbox/kit/shared/protocol";

export const issueStates = ["ready", "parked", "held"] as const;
const LABEL: Record<IssueState, string | null> = {
  ready: null,
  parked: "state:parked",
  held: "state:held",
};
const STATE_MEANING: Record<IssueState, string> = {
  ready: "ready to start",
  parked: "parked: waits on the owner's word",
  held: "held: waits on what the issue names",
};

export const questionInput = z.object({
  /** The decision in one sentence. */
  question: z.string().min(1),
  /** What the owner needs to decide it: the options' costs, what each leads to. */
  context: z.string(),
  issue: z.number().int().positive().optional(),
  /** When the answer is a choice: each option, and the issue's state after it. */
  options: z
    .array(z.object({ label: z.string().min(1), then: z.enum(issueStates).optional() }))
    .max(8)
    .optional(),
});
export const questionAnswer = z.object({
  answer: z.string().min(1),
  /** The chosen option's index, when the answer is one of them. */
  option: z.number().int().nonnegative().optional(),
  /** Where the issue goes, when the question is about one. */
  then: z.enum(issueStates).optional(),
});
export type QuestionInput = z.infer<typeof questionInput>;
export type QuestionAnswer = z.infer<typeof questionAnswer>;
export type Questions = Decisions<QuestionInput, QuestionAnswer>;
type Request = DecisionRequest<QuestionInput, QuestionAnswer>;

/** The answer a choice makes: an option (its label and state), or written text and a state. */
export function answerFor(
  input: QuestionInput,
  choice: { readonly option: number } | { readonly answer: string; readonly then?: IssueState },
): QuestionAnswer {
  if ("option" in choice) {
    const option = input.options?.[choice.option];
    if (!option) throw new Error(`The question has no option ${choice.option + 1}`);
    return {
      answer: option.label,
      option: choice.option,
      ...(input.issue ? { then: option.then ?? "ready" } : {}),
    };
  }
  const answer = choice.answer.trim();
  if (!answer) throw new Error("An answer needs text");
  return { answer, ...(input.issue ? { then: choice.then ?? "ready" } : {}) };
}

/** Each question's GitHub effects: asked (labelled), answered (posted), or why the last try failed. */
export type Effects = Readonly<
  Record<string, { readonly labelled?: number; readonly posted?: number; readonly failed?: string }>
>;

/** The list the TUI, web view and sbn show; pure, so it can be tested on recorded readings. */
export function needsView(
  requests: readonly Request[],
  issues: readonly Issue[],
  issuesReadAt: number,
  effects: Effects,
): NeedsView {
  const byNumber = new Map(issues.map((issue) => [issue.number, issue]));
  const view = (request: Request): QuestionView => {
    const issue = request.input.issue === undefined ? undefined : byNumber.get(request.input.issue);
    const effect = effects[request.id] ?? {};
    return {
      id: request.id,
      revision: request.revision,
      status: request.status,
      question: request.input.question,
      context: request.input.context,
      issue:
        request.input.issue === undefined
          ? null
          : {
              number: request.input.issue,
              title: issue?.title ?? "",
              url: issue?.url ?? "",
            },
      options: request.input.options ?? [],
      askedBy: request.id.startsWith("owner/") ? "owner" : "coordinator",
      requestedAt: request.requestedAt,
      answer: request.result?.decision.answer ?? null,
      then: request.result?.decision.then ?? null,
      answeredAt: request.result?.time ?? null,
      posted:
        request.status !== "decided" || request.input.issue === undefined
          ? null
          : effect.posted
            ? "done"
            : (effect.failed ?? "waiting"),
    };
  };
  // An issue waits for a question unless one is open about it, or an answer about
  // it reached GitHub after the issues were last read (its label is already gone).
  const covered = new Set(
    requests
      .filter(
        (request) =>
          request.input.issue !== undefined &&
          (request.status === "pending" ||
            (request.status === "decided" &&
              (effects[request.id]?.posted === undefined ||
                effects[request.id]!.posted! >= issuesReadAt))),
      )
      .map((request) => request.input.issue!),
  );
  return {
    questions: requests
      .filter((request) => request.status === "pending")
      .sort((a, b) => a.requestedAt - b.requestedAt)
      .map(view),
    unasked: issues
      .filter((issue) => issue.state === "needs-user" && !covered.has(issue.number))
      .map((issue) => ({ number: issue.number, title: issue.title, url: issue.url })),
    answered: requests
      .filter((request) => request.status === "decided")
      .sort((a, b) => (b.result?.time ?? 0) - (a.result?.time ?? 0))
      .slice(0, 10)
      .map(view),
  };
}

/** The comment an answer posts on its issue. */
export function answerComment(request: Request, at: Date): string {
  const decision = request.result!.decision;
  return [
    `**Decided by the owner** · ${at.toISOString().slice(0, 10)}`,
    "",
    `> ${request.input.question.replace(/\n/g, "\n> ")}`,
    "",
    decision.answer,
    ...(decision.then ? ["", `Then: **${STATE_MEANING[decision.then]}**.`] : []),
    "",
    `<sub>Answered in Signalbox; ${request.id.startsWith("owner/") ? "the owner" : "the coordinator"} asked it.</sub>`,
  ].join("\n");
}

/** Runs gh against the project's repository; returns stdout. */
export type GitHub = (args: readonly string[]) => Promise<string>;
export function gitHub(repo: string): GitHub {
  const run = promisify(execFile);
  return async (args) =>
    (await run("gh", [...args, "--repo", repo], { timeout: 60_000, maxBuffer: 8 << 20 })).stdout;
}

const NAMESPACE = "nts/needs";
const RETRY_MS = 5 * 60_000;

export function needs(options: {
  readonly state: Capability<WorkflowStore>;
  readonly questions: Capability<Questions>;
  readonly issues: Capability<Readable<Issue[]>>;
  readonly gitHub: GitHub;
  /** Reads the issues again after a label change, so views show it at once. */
  readonly refreshIssues: () => Promise<void>;
  /** How long a failed write waits before it is tried again (default five minutes). */
  readonly retryMs?: number;
}): { readonly extension: Extension; readonly view: Capability<Readable<NeedsView>> } {
  const viewKey = capability<Readable<NeedsView>>("nts/needs-view");
  const retry = options.retryMs ?? RETRY_MS;
  const effects = new Cell<Effects>({});
  let view: Readable<NeedsView> | undefined;
  const lazyView: Readable<NeedsView> = {
    get: () => view!.get(),
    subscribe: (listener) => view!.subscribe(listener),
  };
  return {
    view: viewKey,
    extension: {
      id: "nts-needs",
      requires: [options.state, options.questions, options.issues],
      provides: [provide(viewKey, lazyView)],
      async start(context) {
        const store = context.get(options.state);
        const questions = context.get(options.questions);
        const issues = context.get(options.issues);
        effects.set(
          await store.transaction(NAMESPACE, (tx) => (tx.get("effects") ?? {}) as Effects),
        );
        /** A done effect clears the last failure; a failure keeps what was done. */
        const record = (id: string, change: Effects[string]) =>
          store
            .transaction(NAMESPACE, (tx) => {
              const all: Record<string, Effects[string]> = {
                ...((tx.get("effects") ?? {}) as Effects),
              };
              const { failed: _previous, ...done } = all[id] ?? {};
              all[id] =
                change.failed === undefined ? { ...done, ...change } : { ...all[id], ...change };
              tx.set("effects", all as unknown as Json);
              return all;
            })
            .then((all) => effects.set(all));
        view = combineReadables(
          [questions.requests, issues, effects],
          (requests, listed, done): NeedsView => {
            const reading = issues.get();
            return needsView(
              requests,
              listed,
              reading.status === "ready" ? reading.observedAt : 0,
              done,
            );
          },
        );
        const labels = async (issue: number) =>
          (
            JSON.parse(
              await options.gitHub(["issue", "view", String(issue), "--json", "labels"]),
            ) as {
              labels: { name: string }[];
            }
          ).labels.map((label) => label.name);
        let running: Promise<void> | undefined;
        let lastTry = new Map<string, number>();
        const apply = async () => {
          const now = Date.now();
          const done = latestValue(effects) ?? {};
          let changed = false;
          for (const request of latestValue(questions.requests) ?? []) {
            const issue = request.input.issue;
            if (issue === undefined) continue;
            const effect = done[request.id] ?? {};
            const owed =
              request.status === "pending"
                ? !effect.labelled
                : request.status === "decided" && !effect.posted;
            if (!owed) continue;
            // A failed write is retried on the next pass after RETRY_MS, not in a tight loop.
            if (effect.failed && now - (lastTry.get(request.id) ?? 0) < retry) continue;
            lastTry.set(request.id, now);
            try {
              if (request.status === "pending") {
                if (!(await labels(issue)).includes("state:needs-user"))
                  await options.gitHub([
                    "issue",
                    "edit",
                    String(issue),
                    "--add-label",
                    "state:needs-user",
                  ]);
                await record(request.id, { labelled: Date.now() });
              } else {
                await options.gitHub([
                  "issue",
                  "comment",
                  String(issue),
                  "--body",
                  answerComment(request, new Date(request.result!.time)),
                ]);
                const have = await labels(issue);
                const then = request.result!.decision.then ?? "ready";
                const edit = [
                  ...(have.includes("state:needs-user")
                    ? ["--remove-label", "state:needs-user"]
                    : []),
                  ...(LABEL[then] && !have.includes(LABEL[then]!)
                    ? ["--add-label", LABEL[then]!]
                    : []),
                ];
                if (edit.length) await options.gitHub(["issue", "edit", String(issue), ...edit]);
                await record(request.id, { posted: Date.now() });
              }
              changed = true;
            } catch (error) {
              await record(request.id, {
                failed: `${error instanceof Error ? error.message : String(error)}`.slice(0, 300),
              });
              context.log.error(`needs: issue #${issue}`, error);
            }
          }
          if (changed) await options.refreshIssues().catch(() => undefined);
        };
        const kick = () => {
          running ??= apply().finally(() => (running = undefined));
        };
        context.afterStart(() => {
          const stop = questions.requests.subscribe(kick);
          const timer = setInterval(kick, retry);
          kick();
          context.onStop(async () => {
            stop();
            clearInterval(timer);
            lastTry = new Map();
            await running;
          });
        });
      },
    },
  };
}
