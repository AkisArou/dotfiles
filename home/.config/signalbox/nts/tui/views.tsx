/** @jsxImportSource @opentui/react */
import type { TaskAction } from "@signalbox/kit/shared/actions";
import type { Project } from "../shared/use-project.ts";
import type { Agent, AssignmentView, Issue, Tone } from "../shared/model.ts";
import {
  agentTone,
  assignmentPhase,
  assignmentSays,
  assignmentTone,
  gaugeTone,
  issueStateTerm,
  issueTone,
  modelsLine,
  needsCount,
  phaseLabel,
  taskMeaning,
} from "../shared/model.ts";
import { gaugeState } from "../status/model.ts";
import { Heading, Line, Section } from "./components.tsx";
import { colors as c } from "./theme.ts";

export const stateMark = (state: string) =>
  state === "ready" ? "○" : state === "needs-user" ? "?" : state === "orphaned" ? "◌" : "◐";
/** A tone in the TUI's palette; the web maps the same tones to its own. */
export const toneColor = (tone: Tone) =>
  ({
    good: c.success,
    active: c.accent,
    attention: c.warning,
    bad: c.danger,
    quiet: c.muted,
    plain: c.text,
  })[tone];
export const agentColor = (state: string) => toneColor(agentTone(state));
export const stateColor = (state: string) => toneColor(issueTone(state));

export function NowView({ project }: { project: Project }) {
  // Ended sessions are history; the Agents tab shows them on request.
  const live = project.data.agents.filter((agent) => agent.state !== "ended");
  const needs = project.data.needs;
  return (
    <>
      <Section
        title={`Needs you: ${needsCount(needs)}${needsCount(needs) ? " · 2 answers them" : ""}`}
      >
        {needs.questions.slice(0, 6).map((question) => (
          <Line key={question.id} color={c.warning}>
            {`? ${question.issue ? `#${question.issue.number} ` : ""}${question.question}`}
          </Line>
        ))}
        {needs.unasked.slice(0, 6).map((issue) => (
          <Line key={issue.number} color={c.warning}>
            {`? #${issue.number} ${issue.title} (not asked yet)`}
          </Line>
        ))}
        {!needsCount(needs) && (
          <Line color={c.muted}>
            {project.issues.status === "ready"
              ? "Nothing waits on you."
              : "Waiting for GitHub issues…"}
          </Line>
        )}
      </Section>
      <Section title="Health">
        {project.nts.health.gauges
          .filter((gauge) => gaugeState(gauge) !== "ok")
          .map((gauge) => (
            <Line
              key={`${gauge.section}/${gauge.id}`}
              color={toneColor(gaugeTone(gaugeState(gauge)))}
            >{`${gauge.label}: ${gauge.text ?? gauge.value}${gauge.why ? ` · ${gauge.why}` : ""}`}</Line>
          ))}
        {!project.nts.readAt && <Line color={c.muted}>Waiting for NTS observations…</Line>}
      </Section>
      <Section
        title={`Agents: ${live.length}${live.length < project.data.agents.length ? ` · ${project.data.agents.length - live.length} ended (Agents tab, e)` : ""}`}
      >
        {live.map((agent) => (
          <Line key={agent.id}>{`${agent.name}  ${agent.state} · ${agent.path ?? ""}`}</Line>
        ))}
        {!project.data.agents.length && (
          <Line color={c.muted}>Open or attach a native terminal in Agents.</Line>
        )}
      </Section>
      <Section
        title={`Assignments: ${project.data.assignments.filter((a) => a.status === "active" || a.status === "queued").length}`}
      >
        {project.data.assignments
          .filter((a) => a.status === "active" || a.status === "queued")
          .map((a) => (
            <Line
              key={a.id}
              color={toneColor(assignmentTone(a))}
            >{`#${a.issue ?? "?"}  ${assignmentPhase(a)} · ${assignmentSays(a)}`}</Line>
          ))}
        {project.data.settings && (
          <Line
            color={c.muted}
          >{`workers ${modelsLine(project.data.settings.roles.worker)} · reviewers ${modelsLine(project.data.settings.roles.reviewer)} · coordinator ${modelsLine(project.data.settings.roles.coordinator)}`}</Line>
        )}
      </Section>
      <Section title="Streams">
        {[...new Set(project.data.issues.map((issue) => issue.stream))].map((stream) => {
          const issues = project.data.issues.filter((issue) => issue.stream === stream);
          return (
            <Line
              key={stream}
            >{`${stream}  ${issues.length} open · ${issues.filter((issue) => issue.state === "in_progress").length} in progress`}</Line>
          );
        })}
      </Section>
    </>
  );
}
/**
 * An issue with its task: what you can do (a), where the task is and what it waits for,
 * then the issue itself, then the task's long notes (its latest instruction, summary and
 * design), then its earlier tasks: the short and actionable first, the long last.
 */
export function TaskDetail({
  issue,
  task,
  earlier = [],
  actions = [],
}: {
  issue: Issue;
  task?: AssignmentView | undefined;
  earlier?: readonly AssignmentView[];
  actions?: readonly TaskAction[];
}) {
  return (
    <>
      {actions.length > 0 && (
        <Line
          color={c.accent}
        >{`a: ${actions.map((action) => action.label.toLowerCase()).join(" · ")}`}</Line>
      )}
      {task && <TaskStatus number={issue.number} assignment={task} />}
      {(task || actions.length > 0) && <box height={1} />}
      <Heading>{`#${issue.number} ${issue.title}`}</Heading>
      <Line color={stateColor(issue.state)}>
        {`${stateMark(issue.state)} ${issueStateTerm(issue.state).label}`}
        <span fg={c.muted}>{`  ${issueStateTerm(issue.state).meaning}`}</span>
      </Line>
      <Line color={c.accent}>{issue.url}</Line>
      {issue.workspace && <Line color={c.accent}>{issue.workspace}</Line>}
      {issue.claims?.map((claim) => (
        <Line key={`${claim.what}/${claim.path}`} color={c.muted}>
          {claim.what}
        </Line>
      ))}
      <Line color={c.muted}>{`labels: ${issue.labels.join(", ") || "none"}`}</Line>
      {issue.owner && <Line>{`owner: ${issue.owner}`}</Line>}
      {issue.parent && <Line>{`parent: #${issue.parent.number} ${issue.parent.title}`}</Line>}
      {issue.progress && (
        <Line>{`${issue.progress.completed}/${issue.progress.total} sub-issues complete`}</Line>
      )}
      {issue.blockedBy.length > 0 && (
        <Line
          color={c.warning}
        >{`blocked by ${issue.blockedBy.map((number) => `#${number}`).join(", ")}`}</Line>
      )}
      <box height={1} />
      <Line>{issue.body}</Line>
      {task && <TaskNotes assignment={task} />}
      {earlier.length > 0 && (
        <>
          <box height={1} />
          <Heading>Earlier tasks</Heading>
          {earlier.map((a) => (
            <Line key={a.id} color={c.muted}>
              {`${a.status} at ${phaseLabel(a.phase ?? a.status)} · round ${a.rounds + 1}${a.commit ? ` · ${a.commit.slice(0, 12)}` : ""}${a.error ? ` · ${a.error.split("\n", 1)[0]}` : ""}`}
            </Line>
          ))}
        </>
      )}
    </>
  );
}
/** An agent's task over everything else, as the Tasks tab shows it. */
export function TaskPopup({
  number,
  issue,
  assignment,
}: {
  number: number;
  issue: Issue | undefined;
  assignment: AssignmentView | undefined;
}) {
  if (issue) return <TaskDetail issue={issue} task={assignment} />;
  return (
    <>
      {assignment ? (
        <>
          <TaskStatus number={number} assignment={assignment} />
          <TaskNotes assignment={assignment} />
          <box height={1} />
        </>
      ) : (
        <Line color={c.muted}>{`No task is running for #${number}.`}</Line>
      )}
      <Line color={c.muted}>{`#${number} is not an open issue.`}</Line>
    </>
  );
}

/** A task in a few lines: its word and round, what it waits for, its tree and commit. */
export function TaskStatus({ number, assignment }: { number: number; assignment: AssignmentView }) {
  return (
    <>
      <Heading>{`#${number} · ${assignmentPhase(assignment)} · round ${assignment.rounds + 1}`}</Heading>
      <Line color={c.muted}>{taskMeaning(assignment)}</Line>
      <Line color={toneColor(assignmentTone(assignment))}>{assignmentSays(assignment)}</Line>
      {assignment.tree && <Line color={c.accent}>{assignment.tree}</Line>}
      {assignment.commit && (
        <Line color={c.muted}>{`commit ${assignment.commit.slice(0, 12)}`}</Line>
      )}
    </>
  );
}
/** A task's long notes: its latest instruction, summary and design. */
export function TaskNotes({ assignment }: { assignment: AssignmentView }) {
  const block = (title: string, text: string) =>
    text ? (
      <>
        <box height={1} />
        <Heading>{title}</Heading>
        <Line>{text}</Line>
      </>
    ) : null;
  return (
    <>
      {block("Latest instruction", assignment.note)}
      {block("Summary", assignment.summary)}
      {block("Design", assignment.design)}
    </>
  );
}
export function AgentDetail({ agent }: { agent: Agent }) {
  return (
    <Section title={agent.name}>
      <Line>{`${agent.state} · ${agent.path ?? ""}`}</Line>
      <Line color={c.muted}>No native terminal is attached.</Line>
    </Section>
  );
}
