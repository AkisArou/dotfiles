// Needs you: every question waiting on the owner, with its choices or a box for
// a written answer; the issues that wait on the owner before anyone asked; and
// what was answered lately, with whether the answer reached its issue.
import { useState } from "react";
import type { Project } from "../../shared/use-project.ts";
import type { IssueState, QuestionView } from "../../shared/model.ts";
import { issueStateLabels } from "../../shared/model.ts";
import { Action, Badge, Empty, Section } from "../components.tsx";

const states: IssueState[] = ["ready", "parked", "held"];
const when = (time: number) =>
  new Date(time).toLocaleString(undefined, { dateStyle: "medium", timeStyle: "short" });

function IssueLink({ issue }: { issue: { number: number; title: string; url: string } }) {
  return (
    <a href={issue.url} target="_blank" rel="noreferrer">
      #{issue.number} {issue.title}
    </a>
  );
}

/** A written answer and, about an issue, where the issue goes after it. */
function WrittenAnswer({
  project,
  issue,
  submit,
}: {
  project: Project;
  issue: boolean;
  submit(answer: string, then: IssueState): Promise<unknown>;
}) {
  const [answer, setAnswer] = useState("");
  const [then, setThen] = useState<IssueState>("ready");
  return (
    <div className="decision">
      <textarea
        placeholder="Write your answer"
        value={answer}
        onChange={(event) => setAnswer(event.target.value)}
      />
      {issue && (
        <label>
          Then the issue is{" "}
          <select value={then} onChange={(event) => setThen(event.target.value as IssueState)}>
            {states.map((state) => (
              <option key={state} value={state}>
                {issueStateLabels[state]}
              </option>
            ))}
          </select>
        </label>
      )}
      <Action
        project={project}
        primary
        disabled={!answer.trim()}
        onClick={async () => {
          await submit(answer.trim(), then);
          setAnswer("");
        }}
      >
        Answer
      </Action>
    </div>
  );
}

function Question({ project, question }: { project: Project; question: QuestionView }) {
  return (
    <div className="decision">
      <strong>{question.question}</strong>
      <p className="subtle">
        {question.issue && (
          <>
            About <IssueLink issue={question.issue} /> ·{" "}
          </>
        )}
        asked by the {question.askedBy} · {when(question.requestedAt)}
      </p>
      {question.context && <p style={{ whiteSpace: "pre-wrap" }}>{question.context}</p>}
      {question.options.length > 0 && (
        <div className="harnesses">
          {question.options.map((option, index) => (
            <Action
              key={index}
              project={project}
              primary={index === 0}
              onClick={() =>
                project.commands.answerQuestion.run({
                  id: question.id,
                  revision: question.revision,
                  option: index,
                })
              }
            >
              {question.issue
                ? `${option.label} → ${issueStateLabels[option.then ?? "ready"]}`
                : option.label}
            </Action>
          ))}
        </div>
      )}
      <WrittenAnswer
        project={project}
        issue={!!question.issue}
        submit={(answer, then) =>
          project.commands.answerQuestion.run({
            id: question.id,
            revision: question.revision,
            answer,
            ...(question.issue ? { then } : {}),
          })
        }
      />
      {question.issue && (
        <p className="subtle">
          Your answer is posted on #{question.issue.number} and the coordinator is told.
        </p>
      )}
    </div>
  );
}

export function NeedsYou({ project }: { project: Project }) {
  const { questions, unasked, answered } = project.data.needs;
  return (
    <>
      <Section title={`Questions · ${questions.length}`}>
        {questions.map((question) => (
          <Question key={question.id} project={project} question={question} />
        ))}
        {!questions.length && <Empty>No open questions.</Empty>}
      </Section>
      {unasked.length > 0 && (
        <Section title={`Waiting on you, not asked yet · ${unasked.length}`}>
          {unasked.map((issue) => (
            <div key={issue.number} className="decision">
              <strong>
                <IssueLink issue={issue} />
              </strong>
              <p className="subtle">
                Labelled as waiting on you; the coordinator is asked to state its question. You can
                answer it now.
              </p>
              <WrittenAnswer
                project={project}
                issue
                submit={(answer, then) =>
                  project.commands.answerIssue.run({ issue: issue.number, answer, then })
                }
              />
            </div>
          ))}
        </Section>
      )}
      {answered.length > 0 && (
        <Section title="Answered lately">
          {answered.map((question) => (
            <div key={question.id} className="decision">
              <strong>{question.question}</strong>
              <p>{question.answer}</p>
              <p className="subtle">
                {question.answeredAt ? when(question.answeredAt) : ""}
                {question.issue && (
                  <>
                    {" "}
                    · <IssueLink issue={question.issue} />
                    {question.then ? ` is ${issueStateLabels[question.then]}` : ""} ·{" "}
                    <Badge
                      state={
                        question.posted === "done"
                          ? "ready"
                          : question.posted === "waiting"
                            ? "busy"
                            : "red"
                      }
                    >
                      {question.posted === "done"
                        ? "posted"
                        : question.posted === "waiting"
                          ? "posting"
                          : `not posted yet: ${question.posted}`}
                    </Badge>
                  </>
                )}
              </p>
            </div>
          ))}
        </Section>
      )}
    </>
  );
}
