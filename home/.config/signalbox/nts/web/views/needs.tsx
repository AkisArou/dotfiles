// Needs you: every question waiting on the owner, with its choices or a written
// answer; the issues that wait on the owner before anyone asked; and what was
// answered lately, with whether the answer reached its issue.
import { useState } from "react";
import { HStack } from "@astryxdesign/core/HStack";
import { Selector } from "@astryxdesign/core/Selector";
import { Table, pixel, proportional } from "@astryxdesign/core/Table";
import { Text } from "@astryxdesign/core/Text";
import { TextArea } from "@astryxdesign/core/TextArea";
import { Timestamp } from "@astryxdesign/core/Timestamp";
import type { Project } from "../../shared/use-project.ts";
import type { IssueState, QuestionView } from "../../shared/model.ts";
import { issueStateLabels, postedTone } from "../../shared/model.ts";
import { Action, Empty, Section, Status } from "../components.tsx";

const states: IssueState[] = ["ready", "parked", "held"];

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
  issue: number | undefined;
  submit(answer: string, then: IssueState): Promise<unknown>;
}) {
  const [answer, setAnswer] = useState("");
  const [then, setThen] = useState<IssueState>("ready");
  return (
    <div className="write">
      <TextArea
        label="Your answer"
        isLabelHidden
        placeholder={issue ? `Your answer, posted on #${issue}` : "Your answer"}
        value={answer}
        onChange={setAnswer}
      />
      {issue !== undefined && (
        <Selector
          label="Then the issue is"
          value={then}
          onChange={(value) => setThen(value as IssueState)}
          options={states.map((state) => ({ value: state, label: issueStateLabels[state] }))}
        />
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
    <div className="question">
      <Text type="large" weight="medium">
        {question.question}
      </Text>
      <Text type="supporting" color="secondary">
        {question.issue && (
          <>
            <IssueLink issue={question.issue} /> ·{" "}
          </>
        )}
        asked by the {question.askedBy} ·{" "}
        <Timestamp value={question.requestedAt / 1000} format="relative" />
      </Text>
      {question.context && <div className="context">{question.context}</div>}
      {question.options.length > 0 && (
        <HStack gap={2} wrap="wrap">
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
        </HStack>
      )}
      <WrittenAnswer
        project={project}
        issue={question.issue?.number}
        submit={(answer, then) =>
          project.commands.answerQuestion.run({
            id: question.id,
            revision: question.revision,
            answer,
            ...(question.issue ? { then } : {}),
          })
        }
      />
      <div>
        <Action
          project={project}
          quiet
          onClick={() =>
            project.commands.withdrawQuestion.run({ id: question.id, revision: question.revision })
          }
        >
          Withdraw the question
        </Action>
      </div>
    </div>
  );
}

export function NeedsYou({ project }: { project: Project }) {
  const { questions, unasked, answered } = project.data.needs;
  const rows = answered.map((question) => ({
    id: question.id,
    question: question.question,
    answer: question.answer ?? "",
    issue: question.issue ? `#${question.issue.number}` : "",
    then: question.then ? issueStateLabels[question.then] : "",
    posted: question.posted ?? "",
    at: question.answeredAt ?? 0,
  }));
  return (
    <>
      <Section title="Questions" count={questions.length}>
        {questions.map((question) => (
          <Question key={question.id} project={project} question={question} />
        ))}
        {!questions.length && <Empty>No open questions</Empty>}
      </Section>
      {unasked.length > 0 && (
        <Section title="Waiting on you, not asked yet" count={unasked.length}>
          {unasked.map((issue) => (
            <div key={issue.number} className="question">
              <Text weight="medium">
                <IssueLink issue={issue} />
              </Text>
              <Text type="supporting" color="secondary">
                The coordinator is asked to state its question; you can answer it now.
              </Text>
              <WrittenAnswer
                project={project}
                issue={issue.number}
                submit={(answer, then) =>
                  project.commands.answerIssue.run({ issue: issue.number, answer, then })
                }
              />
            </div>
          ))}
        </Section>
      )}
      {rows.length > 0 && (
        <Section title="Answered lately" count={rows.length}>
          <Table
            data={rows}
            idKey="id"
            density="compact"
            dividers="rows"
            textOverflow="truncate"
            columns={[
              {
                key: "at",
                header: "When",
                width: pixel(120),
                renderCell: (row) => <Timestamp value={row.at / 1000} format="relative" />,
              },
              {
                key: "question",
                header: "Question",
                width: proportional(3),
                renderCell: (row) => <span title={row.question}>{row.question}</span>,
              },
              {
                key: "answer",
                header: "Answer",
                width: proportional(3),
                renderCell: (row) => <span title={row.answer}>{row.answer}</span>,
              },
              {
                key: "issue",
                header: "Issue",
                width: pixel(170),
                renderCell: (row) =>
                  row.issue ? (
                    <span className="nowrap">
                      <span className="mono">{row.issue}</span>{" "}
                      <span className="secondary">{row.then}</span>
                    </span>
                  ) : null,
              },
              {
                key: "posted",
                header: "On the issue",
                width: pixel(130),
                renderCell: (row) =>
                  row.issue ? (
                    <Status tone={postedTone(row.posted)}>
                      {row.posted === "done"
                        ? "posted"
                        : row.posted === "waiting"
                          ? "posting"
                          : "not yet"}
                    </Status>
                  ) : null,
              },
            ]}
          />
        </Section>
      )}
    </>
  );
}
