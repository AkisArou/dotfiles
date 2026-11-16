import type { Project } from "../../shared/use-project.ts";
import type { Tab } from "../../shared/model.ts";
import { gaugeState } from "../../status/model.ts";
import { Button } from "@astryxdesign/core/Button";
import { Badge, Empty, Section } from "../components.tsx";

export function Now({ project, navigate }: { project: Project; navigate(tab: Tab): void }) {
  const needsYou = project.data.issues.filter((issue) => issue.state === "needs-user");
  const gauges = project.nts.health.gauges.filter((gauge) => gaugeState(gauge) !== "ok");
  return (
    <>
      <Section title={`Needs you · ${needsYou.length}`}>
        {needsYou.slice(0, 8).map((issue) => (
          <p key={issue.id}>
            <a href={issue.url} target="_blank" rel="noreferrer">
              #{issue.number} {issue.title}
            </a>
          </p>
        ))}
        {!needsYou.length && (
          <Empty>
            {project.issues.status === "ready"
              ? "No open questions."
              : "Waiting for GitHub issues…"}
          </Empty>
        )}
      </Section>
      <Section
        title="Health"
        aside={
          <Button label="All health readings" variant="ghost" onClick={() => navigate("Health")} />
        }
      >
        {gauges.map((gauge) => (
          <p key={`${gauge.section}/${gauge.id}`}>
            <Badge state={gaugeState(gauge)}>{gaugeState(gauge)}</Badge> {gauge.label}:{" "}
            {gauge.text ?? gauge.value}
            {gauge.why ? ` · ${gauge.why}` : ""}
          </p>
        ))}
        {!gauges.length && (
          <Empty>
            {project.nts.readAt
              ? "No reported threshold crossings. Check individual sources for missing readings."
              : "Waiting for NTS observations…"}
          </Empty>
        )}
      </Section>
      <Section title={`Agents · ${project.data.agents.length}`}>
        {project.data.agents.map((agent) => (
          <p key={agent.id}>
            {agent.name} <Badge state={agent.state}>{agent.state}</Badge>{" "}
            <span className="subtle">{agent.path}</span>
          </p>
        ))}
        {!project.data.agents.length && (
          <Empty>
            Open a native session in Agents, or attach an existing terminal in config.ts.
          </Empty>
        )}
      </Section>
      <Section title="Streams">
        {[...new Set(project.data.issues.map((issue) => issue.stream))].map((stream) => {
          const issues = project.data.issues.filter((issue) => issue.stream === stream);
          return (
            <p key={stream}>
              {stream}{" "}
              <span className="subtle">
                {issues.length} open ·{" "}
                {issues.filter((issue) => issue.state === "in_progress").length} in progress
              </span>
            </p>
          );
        })}
      </Section>
    </>
  );
}
