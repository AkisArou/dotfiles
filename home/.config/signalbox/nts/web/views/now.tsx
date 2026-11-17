// Now: what needs you, the tasks in flight and what each waits for, what needs
// attention, the live agents, and the streams' size. Everything is one line.
import { agentStates } from "../../shared/glossary.ts";
import { useNavigate } from "@tanstack/react-router";
import { Button } from "@astryxdesign/core/Button";
import { List, ListItem } from "@astryxdesign/core/List";
import { Table, pixel, proportional } from "@astryxdesign/core/Table";
import type { Project } from "../../shared/use-project.ts";
import type { Tab } from "../../shared/model.ts";
import {
  agentTone,
  assignmentPhase,
  assignmentSays,
  assignmentTone,
  gaugeTone,
  needsCount,
  taskMeaning,
} from "../../shared/model.ts";
import { gaugeState } from "../../status/model.ts";
import { Empty, Section, Status } from "../components.tsx";
import { viewPath } from "../project.ts";

const SEVERITY = { red: 0, amber: 1, ok: 2 } as const;

export function Now({ project }: { project: Project }) {
  const navigate = useNavigate();
  const { needs, assignments, agents, issues } = project.data;
  const flight = assignments
    .filter((a) => a.status === "active" || a.status === "queued")
    .map((a) => ({
      id: a.id,
      issue: a.issue ?? 0,
      tone: assignmentTone(a),
      meaning: taskMeaning(a),
      when: assignmentPhase(a),
      says: assignmentSays(a).split("\n", 1)[0] ?? "",
    }))
    .sort((a, b) => a.issue - b.issue);
  const attention = project.nts.health.gauges
    .filter((gauge) => gaugeState(gauge) !== "ok")
    .sort((a, b) => SEVERITY[gaugeState(a)] - SEVERITY[gaugeState(b)]);
  const live = agents
    .filter((agent) => agent.state !== "ended")
    .map((agent) => ({
      id: agent.id,
      name: agent.name,
      state: agent.state,
      path: agent.path ?? "",
    }));
  const streams = [...new Set(issues.map((issue) => issue.stream))].map((stream) => {
    const open = issues.filter((issue) => issue.stream === stream);
    return {
      id: stream,
      stream,
      open: open.length,
      progress: open.filter((issue) => issue.state === "in_progress").length,
    };
  });
  const go = (tab: Tab, label: string) => (
    <Button
      label={label}
      variant="ghost"
      size="sm"
      onClick={() => void navigate({ to: viewPath[tab] })}
    >
      {label}
    </Button>
  );
  return (
    <div className="grid">
      <div className="column">
        <Section title="Needs you" count={needsCount(needs)} aside={go("Needs you", "Answer")}>
          {needsCount(needs) ? (
            <List density="compact" hasDividers edge="padded">
              {needs.questions.map((question) => (
                <ListItem
                  key={question.id}
                  label={question.question}
                  description={
                    question.issue ? `#${question.issue.number} ${question.issue.title}` : undefined
                  }
                />
              ))}
              {needs.unasked.map((issue) => (
                <ListItem
                  key={issue.number}
                  label={`#${issue.number} ${issue.title}`}
                  description="waits on you; not asked yet"
                />
              ))}
            </List>
          ) : (
            <Empty>Nothing waits on you</Empty>
          )}
        </Section>
        <Section title="In flight" count={flight.length} aside={go("Agents", "Agents")}>
          {flight.length ? (
            <Table
              data={flight}
              idKey="id"
              density="compact"
              dividers="rows"
              hasHover
              textOverflow="truncate"
              columns={[
                {
                  key: "issue",
                  header: "Task",
                  width: pixel(72),
                  renderCell: (row) => (
                    <button
                      type="button"
                      className="cell-link mono"
                      title={`Open #${row.issue}`}
                      onClick={() =>
                        void navigate({
                          to: ".",
                          search: (previous: Record<string, unknown>) => ({
                            ...previous,
                            issue: row.issue,
                          }),
                        })
                      }
                    >
                      #{row.issue}
                    </button>
                  ),
                },
                {
                  key: "phase",
                  header: "Phase",
                  width: pixel(150),
                  renderCell: (row) => (
                    <Status tone={row.tone} title={row.meaning}>
                      {row.when}
                    </Status>
                  ),
                },
                {
                  key: "says",
                  header: "Waiting for",
                  width: proportional(3),
                  renderCell: (row) => <span title={row.says}>{row.says}</span>,
                },
              ]}
            />
          ) : (
            <Empty>No task is running</Empty>
          )}
        </Section>
      </div>
      <div className="column">
        <Section title="Attention" count={attention.length} aside={go("Health", "All readings")}>
          {attention.length ? (
            <List density="compact" hasDividers edge="padded">
              {attention.map((gauge) => (
                <ListItem
                  key={`${gauge.section}/${gauge.id}`}
                  startContent={<Status tone={gaugeTone(gaugeState(gauge))}>{""}</Status>}
                  label={`${gauge.label}: ${gauge.text ?? `${gauge.value}${gauge.unit ? ` ${gauge.unit}` : ""}`}`}
                  description={gauge.why}
                />
              ))}
            </List>
          ) : (
            <Empty>
              {project.nts.readAt
                ? "No reading crosses its threshold"
                : "Waiting for NTS readings…"}
            </Empty>
          )}
        </Section>
        <Section title="Agents" count={live.length}>
          {live.length ? (
            <Table
              data={live}
              idKey="id"
              density="compact"
              dividers="rows"
              textOverflow="truncate"
              columns={[
                { key: "name", header: "Session", width: proportional(2) },
                {
                  key: "state",
                  header: "State",
                  width: pixel(110),
                  renderCell: (row) => (
                    <Status tone={agentTone(row.state)} title={agentStates[row.state]?.meaning}>
                      {row.state}
                    </Status>
                  ),
                },
                {
                  key: "path",
                  header: "Tree",
                  width: proportional(2),
                  renderCell: (row) => (
                    <span className="mono secondary" title={row.path}>
                      {row.path}
                    </span>
                  ),
                },
              ]}
            />
          ) : (
            <Empty>No live agent</Empty>
          )}
        </Section>
        <Section title="Streams" count={streams.length} aside={go("Tasks", "Tasks")}>
          {streams.length ? (
            <Table
              data={streams}
              idKey="id"
              density="compact"
              dividers="rows"
              textOverflow="truncate"
              columns={[
                { key: "stream", header: "Stream", width: proportional(4) },
                { key: "open", header: "Open", width: pixel(70), align: "end" },
                { key: "progress", header: "In progress", width: pixel(100), align: "end" },
              ]}
            />
          ) : (
            <Empty>Waiting for GitHub issues…</Empty>
          )}
        </Section>
      </div>
    </div>
  );
}
