/** @jsxImportSource @opentui/react */
// The TUI's Needs you: open questions first, then issues that wait on the owner
// before anyone asked, then what was answered lately. Enter answers: a question's
// choices in the finder, or a written answer in the editor; about an issue, the
// finder then asks where the issue goes (ready, parked or held). The host posts
// the answer on the issue and tells the coordinator.
import { mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import type { Project } from "../shared/use-project.ts";
import type { IssueState, NeedsView, QuestionView } from "../shared/model.ts";
import { issueStateLabels, postedTone } from "../shared/model.ts";
import { toneColor } from "./views.tsx";
import { Line, Section } from "./components.tsx";
import { colors as c } from "./theme.ts";
import type { TerminalCommand, TerminalTools } from "./tools.ts";

export type NeedsItem =
  | { readonly id: string; readonly kind: "question"; readonly question: QuestionView }
  | {
      readonly id: string;
      readonly kind: "unasked";
      readonly issue: { number: number; title: string; url: string };
    }
  | { readonly id: string; readonly kind: "answered"; readonly question: QuestionView };

export function needsItems(view: NeedsView): NeedsItem[] {
  return [
    ...view.questions.map((question) => ({
      id: `question:${question.id}`,
      kind: "question" as const,
      question,
    })),
    ...view.unasked.map((issue) => ({
      id: `issue:${issue.number}`,
      kind: "unasked" as const,
      issue,
    })),
    ...view.answered.map((question) => ({
      id: `answered:${question.id}`,
      kind: "answered" as const,
      question,
    })),
  ];
}

/** One line for the list: the question, or the issue waiting for one. */
/** One line for the list: the question (or the issue waiting for one), its issue, and what it wants. */
export function needsLabel(item: NeedsItem): { label: string; meta?: string; end: string } {
  if (item.kind === "unasked")
    return { label: item.issue.title, meta: `#${item.issue.number}`, end: "not asked yet" };
  const meta = item.question.issue ? `#${item.question.issue.number}` : undefined;
  return item.kind === "question"
    ? {
        label: item.question.question,
        ...(meta ? { meta } : {}),
        end: item.question.options.length ? "choose" : "answer",
      }
    : { label: item.question.question, ...(meta ? { meta } : {}), end: posted(item.question) };
}

const posted = (question: QuestionView) =>
  !question.issue
    ? "answered"
    : question.posted === "done"
      ? "posted"
      : question.posted === "waiting"
        ? "posting"
        : "not posted";

const when = (time: number) =>
  new Date(time).toLocaleString(undefined, { dateStyle: "medium", timeStyle: "short" });

export function NeedsDetail({ item }: { item: NeedsItem }) {
  if (item.kind === "unasked")
    return (
      <Section title={`#${item.issue.number} ${item.issue.title}`}>
        <Line color={c.warning}>Waits on you, but nobody has asked you a question yet.</Line>
        <Line color={c.muted}>The coordinator is told to read it and ask. {item.issue.url}</Line>
        <Line color={c.muted}> </Line>
        <Line>Enter answers it now: write what should happen, then where the issue goes.</Line>
      </Section>
    );
  const question = item.question;
  return (
    <Section title={question.question}>
      {question.issue && (
        <Line
          color={c.muted}
        >{`About #${question.issue.number} ${question.issue.title} · ${question.issue.url}`}</Line>
      )}
      <Line
        color={c.muted}
      >{`Asked by the ${question.askedBy} · ${when(question.requestedAt)}`}</Line>
      <Line color={c.muted}> </Line>
      {question.context.split("\n").map((line, index) => (
        <Line key={`context-${index}`}>{line || " "}</Line>
      ))}
      {question.options.length > 0 && <Line color={c.muted}> </Line>}
      {question.options.map((option, index) => (
        <Line key={`option-${index}`}>
          {`${index + 1}. ${option.label}${question.issue ? `  → ${issueStateLabels[option.then ?? "ready"]}` : ""}`}
        </Line>
      ))}
      <Line color={c.muted}> </Line>
      {item.kind === "question" ? (
        <Line>
          {question.options.length
            ? "Enter chooses one, or writes your own answer."
            : "Enter writes your answer in the editor."}
          {question.issue ? ` It is posted on #${question.issue.number}.` : ""}
        </Line>
      ) : (
        <>
          <Line
            color={c.accent}
          >{`Answered ${question.answeredAt ? when(question.answeredAt) : ""}: ${question.answer ?? ""}`}</Line>
          {question.issue && (
            <Line color={toneColor(postedTone(question.posted))}>
              {`#${question.issue.number}${question.then ? ` is ${issueStateLabels[question.then]}` : ""} · ${
                question.posted === "done"
                  ? "posted on the issue"
                  : question.posted === "waiting"
                    ? "posting on the issue…"
                    : `not posted yet (retried): ${question.posted}`
              }`}
            </Line>
          )}
        </>
      )}
    </Section>
  );
}

const STATES: IssueState[] = ["ready", "parked", "held"];

/** Writes an answer in the editor; lines starting with # are the question's notes. */
async function written(
  tools: TerminalTools,
  editor: TerminalCommand,
  name: string,
  notes: readonly string[],
): Promise<string | undefined> {
  const directory = join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "signalbox-tui");
  await mkdir(directory, { recursive: true });
  const file = join(directory, `answer-${name.replace(/[^A-Za-z0-9_.-]/g, "_")}.md`);
  await writeFile(
    file,
    [
      "",
      "# Write your answer above. Lines starting with # are left out; an empty answer cancels.",
      ...notes.map((note) => `# ${note}`),
      "",
    ].join("\n"),
  );
  try {
    await tools.run({ ...editor, args: [...(editor.args ?? []), file] }, directory);
    const text = (await readFile(file, "utf8"))
      .split("\n")
      .filter((line) => !line.startsWith("#"))
      .join("\n")
      .trim();
    return text || undefined;
  } finally {
    await rm(file, { force: true });
  }
}

/** Withdraws an open question after the owner confirms; the coordinator is told. */
export async function withdrawNeed(
  item: NeedsItem,
  project: Project,
  tools: TerminalTools,
): Promise<string | undefined> {
  if (item.kind !== "question") return "Only an open question can be withdrawn.";
  const confirm = await tools.find(
    [`Withdraw: ${item.question.question}`, "Keep it"],
    "Withdraw the question?",
  );
  if (confirm !== 0) return undefined;
  await project.commands.withdrawQuestion.run({
    id: item.question.id,
    revision: item.question.revision,
  });
  return "Withdrawn; the coordinator is told.";
}

/** Answers one item; returns what to tell the owner, or undefined when they cancelled. */
export async function answerNeed(
  item: NeedsItem,
  project: Project,
  tools: TerminalTools,
  editor: TerminalCommand,
): Promise<string | undefined> {
  if (item.kind === "answered") return "Already answered.";
  const issue = item.kind === "unasked" ? item.issue : item.question.issue;
  const where = async () => {
    if (!issue) return undefined;
    const index = await tools.find(
      STATES.map((state) => `#${issue.number} is then ${issueStateLabels[state]}`),
      `Then #${issue.number}`,
    );
    return index === undefined ? null : STATES[index]!;
  };
  const notes =
    item.kind === "unasked"
      ? [
          `#${item.issue.number} ${item.issue.title} waits on you; nobody has asked a question.`,
          item.issue.url,
        ]
      : [
          `Question: ${item.question.question}`,
          ...(item.question.context ? ["", ...item.question.context.split("\n")] : []),
        ];
  if (item.kind === "question" && item.question.options.length) {
    const question = item.question;
    const lines = [
      ...question.options.map(
        (option, index) =>
          `${index + 1}. ${option.label}${question.issue ? `  → ${issueStateLabels[option.then ?? "ready"]}` : ""}`,
      ),
      "✎ Write a different answer",
    ];
    const picked = await tools.find(lines, question.question.slice(0, 60));
    if (picked === undefined) return undefined;
    if (picked < question.options.length) {
      await project.commands.answerQuestion.run({
        id: question.id,
        revision: question.revision,
        option: picked,
      });
      return question.issue
        ? `Answered; it goes on #${question.issue.number}, and the coordinator is told.`
        : "Answered; the coordinator is told.";
    }
  }
  const answer = await written(tools, editor, item.id, notes);
  if (!answer) return undefined;
  const then = await where();
  if (then === null) return undefined;
  if (item.kind === "unasked")
    await project.commands.answerIssue.run({ issue: item.issue.number, answer, then: then! });
  else
    await project.commands.answerQuestion.run({
      id: item.question.id,
      revision: item.question.revision,
      answer,
      ...(then ? { then } : {}),
    });
  return issue
    ? `Answered; it goes on #${issue.number}${then ? `, which is then ${issueStateLabels[then]}` : ""}, and the coordinator is told.`
    : "Answered; the coordinator is told.";
}
