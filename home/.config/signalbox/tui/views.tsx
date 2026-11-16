/** @jsxImportSource @opentui/react */
import type { Project } from "../shared/use-project.ts";
import type { Agent, Issue } from "../shared/model.ts";
import { gaugeState } from "../nts/model.ts";
import { Heading, Line, Section } from "./components.tsx";
import { colors as c } from "./theme.ts";

export const stateMark = (state: string) =>
  state === "ready" ? "○" : state === "needs-user" ? "?" : "◐";
export const stateColor = (state: string) =>
  ["held", "parked", "needs-user", "unscoped"].includes(state) ? c.warning : c.text;

export function NowView({ project }: { project: Project }) {
  const needsYou = project.data.issues.filter((issue) => issue.state === "needs-user");
  return (
    <>
      <Section title={`Needs you: ${needsYou.length}`}>
        {needsYou.slice(0, 6).map((issue) => (
          <Line key={issue.id} color={c.warning}>{`? #${issue.number} ${issue.title}`}</Line>
        ))}
        {!needsYou.length && (
          <Line color={c.muted}>
            {project.issues.status === "ready"
              ? "No open questions."
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
              color={gaugeState(gauge) === "red" ? c.danger : c.warning}
            >{`${gauge.label}: ${gauge.text ?? gauge.value}${gauge.why ? ` · ${gauge.why}` : ""}`}</Line>
          ))}
        {!project.nts.readAt && <Line color={c.muted}>Waiting for NTS observations…</Line>}
      </Section>
      <Section title={`Agents: ${project.data.agents.length}`}>
        {project.data.agents.map((agent) => (
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
            <Line key={a.id} color={a.error || a.help ? c.warning : c.text}>{`#${a.issue ?? "?"}  ${
              a.error ? "faulted" : (a.phase ?? a.status)
            } · ${a.error ?? a.help ?? a.wait ?? ""}`}</Line>
          ))}
        {project.data.profiles && (
          <Line color={c.muted}>{`workers ${project.data.profiles.roles.worker.profile} · reviewers ${project.data.profiles.roles.reviewer.profile} · coordinator ${project.data.profiles.roles.coordinator.profile}`}</Line>
        )}
        {project.data.decisions
          .filter((d) => d.status === "pending")
          .map((d) => (
            <Line key={d.id} color={c.warning}>{`? ${d.question} (answer in the browser's Needs you)`}</Line>
          ))}
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
export function TaskDetail({ issue }: { issue: Issue }) {
  return (
    <>
      <Heading>{`#${issue.number} ${issue.title}`}</Heading>
      <Line color={stateColor(issue.state)}>{`${stateMark(issue.state)} ${issue.state}`}</Line>
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
