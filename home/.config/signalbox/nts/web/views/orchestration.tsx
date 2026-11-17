// Agents' settings and assignments. Settings: settings.json (the only source of
// worker slots, shares, rounds and each role's models and pick) with the host's
// pause and harness availability. Assignments: start a task by hand, and every
// task's phase, wait, rounds and commit.
import { useState } from "react";
import { Selector } from "@astryxdesign/core/Selector";
import { Table, pixel, proportional } from "@astryxdesign/core/Table";
import { TextInput } from "@astryxdesign/core/TextInput";
import { Text } from "@astryxdesign/core/Text";
import { Toolbar } from "@astryxdesign/core/Toolbar";
import type { Project } from "../../shared/use-project.ts";
import type { AgentRole, Harness, Pick } from "../../shared/model.ts";
import { assignmentPhase, assignmentSays, assignmentTone, pickLabels } from "../../shared/model.ts";
import { Action, Empty, Section, Status } from "../components.tsx";

const roleNames: Record<AgentRole, string> = {
  coordinator: "Coordinator",
  worker: "Workers",
  reviewer: "Reviewers",
};
/** The coordinator runs one at a time, so it only takes the first available. */
const picksFor: Record<AgentRole, Pick[]> = {
  coordinator: ["first"],
  worker: ["first", "spread", "coordinator"],
  reviewer: ["first", "spread"],
};
const range = (from: number, to: number) =>
  Array.from({ length: to - from + 1 }, (_, index) => from + index);
const numbers = (values: number[], suffix = "") =>
  values.map((value) => ({ value: String(value), label: `${value}${suffix}` }));

export function Settings({ project }: { project: Project }) {
  const settings = project.data.settings;
  if (!settings) return <Empty>Settings are loading…</Empty>;
  const write = (change: Parameters<typeof project.commands.setSettings.run>[0]) =>
    project.perform(() =>
      project.commands.setSettings.run({ ...change, version: settings.version }),
    );
  const ids = (role: AgentRole) => settings.roles[role].models.map((listed) => listed.id);
  const roleRows = (Object.keys(roleNames) as AgentRole[]).map((role) => ({
    id: role,
    role,
  }));
  const harnessRows = (["claude", "codex"] as Harness[]).map((harness) => ({
    id: harness,
    harness,
    blocked: settings.unavailable[harness]?.reason ?? "",
  }));
  return (
    <>
      <Section title="Running" aside={<span className="mono secondary">{settings.path}</span>}>
        <div className="controls">
          <Selector
            label="Worker slots"
            value={String(settings.workerSlots)}
            onChange={(value) => void write({ workerSlots: Number(value) })}
            options={numbers(range(0, 8))}
          />
          {settings.shares?.map((group) => (
            <Selector
              key={group.name}
              label={`${group.name[0]!.toUpperCase()}${group.name.slice(1)} share · ${group.running} running`}
              value={String(Math.round(group.percent / 10) * 10)}
              onChange={(value) =>
                void write({ share: { name: group.name, percent: Number(value) } })
              }
              options={numbers(
                range(0, 10).map((step) => step * 10),
                "%",
              )}
            />
          ))}
          <Selector
            label="Rounds before escalation"
            value={String(settings.maxRounds)}
            onChange={(value) => void write({ maxRounds: Number(value) })}
            options={numbers(range(1, 10))}
          />
          <div className="controls-end">
            {settings.paused ? (
              <Action project={project} onClick={() => project.commands.resumeTasks.run({})}>
                Resume new tasks
              </Action>
            ) : (
              <Action
                project={project}
                onClick={() =>
                  project.commands.pauseTasks.run({ reason: "paused from the web view" })
                }
              >
                Pause new tasks
              </Action>
            )}
          </div>
        </div>
      </Section>
      <Section
        title="Models"
        aside={
          <Text type="supporting" color="secondary">
            applies to the next launch
          </Text>
        }
      >
        <Table
          data={roleRows}
          idKey="id"
          density="compact"
          dividers="rows"
          verticalAlign="top"
          columns={[
            {
              key: "role",
              header: "Role",
              width: pixel(120),
              renderCell: (row) => roleNames[row.role],
            },
            {
              key: "pick",
              header: "Picks",
              width: proportional(2),
              renderCell: (row) => (
                <Selector
                  label={`How ${roleNames[row.role].toLowerCase()} pick`}
                  isLabelHidden
                  value={settings.roles[row.role].pick}
                  onChange={(value) =>
                    void write({ roles: { [row.role]: { pick: value as Pick } } })
                  }
                  options={picksFor[row.role].map((way) => ({
                    value: way,
                    label: pickLabels[way],
                  }))}
                  isDisabled={picksFor[row.role].length === 1}
                />
              ),
            },
            {
              key: "models",
              header: "Models, in order",
              width: proportional(3),
              renderCell: (row) => (
                <div className="column" style={{ gap: 4 }}>
                  {settings.roles[row.role].models.map((entry, index, listed) => (
                    <div key={entry.id} className="nowrap">
                      <span className="mono secondary">{index + 1}.</span> {entry.label}{" "}
                      {index > 0 && (
                        <Action
                          project={project}
                          quiet
                          onClick={() =>
                            project.commands.setSettings.run({
                              roles: {
                                [row.role]: {
                                  models: [
                                    entry.id,
                                    ...ids(row.role).filter((id) => id !== entry.id),
                                  ],
                                },
                              },
                              version: settings.version,
                            })
                          }
                        >
                          First
                        </Action>
                      )}
                      {listed.length > 1 && (
                        <Action
                          project={project}
                          quiet
                          onClick={() =>
                            project.commands.setSettings.run({
                              roles: {
                                [row.role]: {
                                  models: ids(row.role).filter((id) => id !== entry.id),
                                },
                              },
                              version: settings.version,
                            })
                          }
                        >
                          Remove
                        </Action>
                      )}
                    </div>
                  ))}
                </div>
              ),
            },
            {
              key: "add",
              header: "Add",
              width: proportional(2),
              renderCell: (row) => (
                <Selector
                  label={`Add a model for ${roleNames[row.role].toLowerCase()}`}
                  isLabelHidden
                  placeholder="Add a model…"
                  hasSearch
                  value=""
                  onChange={(value) =>
                    void write({ roles: { [row.role]: { models: [...ids(row.role), value] } } })
                  }
                  options={settings.catalog
                    .filter((entry) => !ids(row.role).includes(entry.id))
                    .map((entry) => ({ value: entry.id, label: entry.label }))}
                />
              ),
            },
          ]}
        />
      </Section>
      <Section title="Harnesses">
        <Table
          data={harnessRows}
          idKey="id"
          density="compact"
          dividers="rows"
          columns={[
            {
              key: "harness",
              header: "Harness",
              width: pixel(160),
              renderCell: (row) => (row.harness === "claude" ? "Claude Code" : "Codex"),
            },
            {
              key: "blocked",
              header: "State",
              width: proportional(2),
              renderCell: (row) =>
                row.blocked ? (
                  <Status tone="attention">{`unavailable: ${row.blocked}`}</Status>
                ) : (
                  <Status tone="plain">available</Status>
                ),
            },
            {
              key: "action",
              header: "",
              width: pixel(170),
              align: "end",
              renderCell: (row) => (
                <Action
                  project={project}
                  quiet
                  onClick={() =>
                    project.commands.setAvailability.run({
                      harness: row.harness,
                      unavailable: row.blocked ? null : { reason: "usage exhausted" },
                    })
                  }
                >
                  {row.blocked ? "Mark available" : "Mark unavailable"}
                </Action>
              ),
            },
          ]}
        />
      </Section>
    </>
  );
}

export function Assignments({ project }: { project: Project }) {
  const settings = project.data.settings;
  const [issue, setIssue] = useState("");
  const [instructions, setInstructions] = useState("");
  const [model, setModel] = useState("");
  const workersNamed = settings?.roles.worker.pick === "coordinator";
  const rows = project.data.assignments.map((assignment) => ({
    id: assignment.id,
    task: assignment.issue ? `#${assignment.issue}` : assignment.key,
    tone: assignmentTone(assignment),
    phase: assignmentPhase(assignment),
    says: assignmentSays(assignment).split("\n", 1)[0] ?? "",
    rounds: assignment.rounds,
    commit: assignment.commit?.slice(0, 12) ?? "",
    open: assignment.status === "active" || assignment.status === "queued",
  }));
  return (
    <>
      <Section title="Assign a task">
        <Toolbar
          label="Assign a task"
          startContent={
            <>
              <TextInput
                label="Issue"
                isLabelHidden
                size="sm"
                placeholder="Issue number"
                value={issue}
                onChange={(value) => setIssue(value.replace(/\D/g, ""))}
              />
              <TextInput
                label="Note for the worker"
                isLabelHidden
                size="sm"
                placeholder="Note for the worker (optional)"
                value={instructions}
                onChange={setInstructions}
              />
              {workersNamed && settings && (
                <Selector
                  label="Model"
                  isLabelHidden
                  value={model}
                  onChange={setModel}
                  placeholder="First available model"
                  options={settings.roles.worker.models.map((entry) => ({
                    value: entry.id,
                    label: entry.label,
                  }))}
                />
              )}
              <Action
                project={project}
                primary
                disabled={!issue}
                onClick={async () => {
                  await project.commands.assignTask.run({
                    issue: Number(issue),
                    instructions,
                    ...(workersNamed && model ? { model } : {}),
                  });
                  setIssue("");
                  setInstructions("");
                  setModel("");
                }}
              >
                Assign
              </Action>
            </>
          }
        />
      </Section>
      <Section title="Assignments" count={rows.length}>
        {rows.length ? (
          <Table
            data={rows}
            idKey="id"
            density="compact"
            dividers="rows"
            hasHover
            textOverflow="truncate"
            columns={[
              {
                key: "task",
                header: "Task",
                width: pixel(80),
                renderCell: (row) => <span className="mono">{row.task}</span>,
              },
              {
                key: "phase",
                header: "Phase",
                width: pixel(160),
                renderCell: (row) => <Status tone={row.tone}>{row.phase}</Status>,
              },
              {
                key: "says",
                header: "Waiting for",
                width: proportional(4),
                renderCell: (row) => <span title={row.says}>{row.says}</span>,
              },
              { key: "rounds", header: "Rounds", width: pixel(80), align: "end" },
              {
                key: "commit",
                header: "Commit",
                width: pixel(130),
                renderCell: (row) => <span className="mono secondary">{row.commit}</span>,
              },
              {
                key: "cancel",
                header: "",
                width: pixel(90),
                align: "end",
                renderCell: (row) =>
                  row.open ? (
                    <Action
                      project={project}
                      quiet
                      onClick={() =>
                        project.commands.cancelAssignment.run({
                          assignment: row.id,
                          reason: "cancelled by the user",
                        })
                      }
                    >
                      Cancel
                    </Action>
                  ) : null,
              },
            ]}
          />
        ) : (
          <Empty>No assignments yet</Empty>
        )}
      </Section>
    </>
  );
}
