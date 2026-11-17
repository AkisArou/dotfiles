// The orchestration controls: settings.json (the only source of worker slots,
// rounds and each role's models, and how one is picked) with the host's pause
// and harness availability, the task assignments, and starting one by hand.
import { useState } from "react";
import type { Project } from "../../shared/use-project.ts";
import type { AgentRole, Harness, Pick } from "../../shared/model.ts";
import { assignmentPhase, assignmentSays, pickLabels } from "../../shared/model.ts";
import { Action, Badge, Empty, Section } from "../components.tsx";

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

export function Orchestration({ project }: { project: Project }) {
  const settings = project.data.settings;
  const [issue, setIssue] = useState("");
  const [instructions, setInstructions] = useState("");
  const [model, setModel] = useState("");
  if (!settings) return <Empty>Settings are loading.</Empty>;
  const write = (change: Parameters<typeof project.commands.setSettings.run>[0]) =>
    project.commands.setSettings.run({ ...change, version: settings.version });
  const models = (role: AgentRole) => settings.roles[role].models.map((listed) => listed.id);
  const workersNamed = settings.roles.worker.pick === "coordinator";
  return (
    <div className="orchestration">
      <Section title="Running" aside={<span className="subtle">{settings.path}</span>}>
        {settings.error && (
          <Badge state="red">{`settings.json is not in force (the last good settings are): ${settings.error}`}</Badge>
        )}
        <div className="harnesses">
          <label>
            Worker slots{" "}
            <select
              value={settings.workerSlots}
              disabled={project.busy}
              onChange={(event) =>
                void project.perform(() => write({ workerSlots: Number(event.target.value) }))
              }
            >
              {range(0, 8).map((n) => (
                <option key={n} value={n}>
                  {n}
                </option>
              ))}
            </select>
          </label>
          <label>
            Rounds before escalation{" "}
            <select
              value={settings.maxRounds}
              disabled={project.busy}
              onChange={(event) =>
                void project.perform(() => write({ maxRounds: Number(event.target.value) }))
              }
            >
              {range(1, 10).map((n) => (
                <option key={n} value={n}>
                  {n}
                </option>
              ))}
            </select>
          </label>
          {settings.paused ? (
            <>
              <Badge state="held">{`Paused: ${settings.paused.reason}`}</Badge>
              <Action project={project} onClick={() => project.commands.resumeTasks.run({})}>
                Resume new tasks
              </Action>
            </>
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
      </Section>
      <Section title="Models" aside={<span className="subtle">APPLIES TO THE NEXT LAUNCH</span>}>
        <table className="table">
          <thead>
            <tr>
              <th>Role</th>
              <th>Picks</th>
              <th>Models, in order</th>
              <th>Add</th>
            </tr>
          </thead>
          <tbody>
            {(Object.keys(roleNames) as AgentRole[]).map((role) => {
              const { pick, models: listed } = settings.roles[role];
              const ids = models(role);
              return (
                <tr key={role}>
                  <td>{roleNames[role]}</td>
                  <td>
                    <select
                      value={pick}
                      disabled={project.busy || picksFor[role].length === 1}
                      onChange={(event) =>
                        void project.perform(() =>
                          write({ roles: { [role]: { pick: event.target.value as Pick } } }),
                        )
                      }
                    >
                      {picksFor[role].map((way) => (
                        <option key={way} value={way}>
                          {pickLabels[way]}
                        </option>
                      ))}
                    </select>
                  </td>
                  <td>
                    {listed.map((entry, index) => (
                      <div key={entry.id} className="harness">
                        <span>{`${index + 1}. ${entry.label}`}</span>
                        {index > 0 && (
                          <Action
                            project={project}
                            onClick={() =>
                              write({
                                roles: {
                                  [role]: {
                                    models: [entry.id, ...ids.filter((id) => id !== entry.id)],
                                  },
                                },
                              })
                            }
                          >
                            First
                          </Action>
                        )}
                        {listed.length > 1 && (
                          <Action
                            project={project}
                            onClick={() =>
                              write({
                                roles: { [role]: { models: ids.filter((id) => id !== entry.id) } },
                              })
                            }
                          >
                            Remove
                          </Action>
                        )}
                      </div>
                    ))}
                  </td>
                  <td>
                    <select
                      value=""
                      disabled={project.busy}
                      onChange={(event) =>
                        void project.perform(() =>
                          write({ roles: { [role]: { models: [...ids, event.target.value] } } }),
                        )
                      }
                    >
                      <option value="">Add a model…</option>
                      {settings.catalog
                        .filter((entry) => !ids.includes(entry.id))
                        .map((entry) => (
                          <option key={entry.id} value={entry.id}>
                            {entry.label}
                          </option>
                        ))}
                    </select>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
        <div className="harnesses">
          {(["claude", "codex"] as Harness[]).map((harness) => {
            const blocked = settings.unavailable[harness];
            return (
              <div key={harness} className="harness">
                <strong>{harness === "claude" ? "Claude Code" : "Codex"}</strong>
                <Badge state={blocked ? "held" : "ready"}>
                  {blocked ? `unavailable: ${blocked.reason}` : "available"}
                </Badge>
                <Action
                  project={project}
                  onClick={() =>
                    project.commands.setAvailability.run({
                      harness,
                      unavailable: blocked ? null : { reason: "usage exhausted" },
                    })
                  }
                >
                  {blocked ? "Mark available" : "Mark unavailable"}
                </Action>
              </div>
            );
          })}
        </div>
      </Section>
      <Section title="Assignments">
        <div className="assign">
          <input
            inputMode="numeric"
            placeholder="Issue number"
            value={issue}
            onChange={(event) => setIssue(event.target.value.replace(/\D/g, ""))}
          />
          <input
            placeholder="Note for the worker (optional)"
            value={instructions}
            onChange={(event) => setInstructions(event.target.value)}
          />
          {workersNamed && (
            <select value={model} onChange={(event) => setModel(event.target.value)}>
              <option value="">First available model</option>
              {settings.roles.worker.models.map((entry) => (
                <option key={entry.id} value={entry.id}>
                  {entry.label}
                </option>
              ))}
            </select>
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
        </div>
        {project.data.assignments.length ? (
          <table className="table">
            <thead>
              <tr>
                <th>Task</th>
                <th>Phase</th>
                <th>Waiting for</th>
                <th>Rounds</th>
                <th>Commit</th>
                <th />
              </tr>
            </thead>
            <tbody>
              {project.data.assignments.map((assignment) => (
                <tr key={assignment.id}>
                  <td>{assignment.issue ? `#${assignment.issue}` : assignment.key}</td>
                  <td>
                    <Badge
                      state={
                        assignment.error
                          ? "error"
                          : assignment.escalation || assignment.stalled || assignment.help
                            ? "needs-user"
                            : assignment.status
                      }
                    >
                      {assignmentPhase(assignment)}
                    </Badge>
                  </td>
                  <td>{assignmentSays(assignment)}</td>
                  <td>{assignment.rounds}</td>
                  <td className="mono">{assignment.commit?.slice(0, 12) ?? ""}</td>
                  <td>
                    {(assignment.status === "active" || assignment.status === "queued") && (
                      <Action
                        project={project}
                        onClick={() =>
                          project.commands.cancelAssignment.run({
                            assignment: assignment.id,
                            reason: "cancelled by the user",
                          })
                        }
                      >
                        Cancel
                      </Action>
                    )}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        ) : (
          <Empty>No assignments yet.</Empty>
        )}
      </Section>
    </div>
  );
}

export function UserDecisions({ project }: { project: Project }) {
  const pending = project.data.decisions.filter((decision) => decision.status === "pending");
  const [answers, setAnswers] = useState<Record<string, string>>({});
  if (!pending.length) return null;
  return (
    <Section title="Questions from the coordinator">
      {pending.map((decision) => (
        <div key={decision.id} className="decision">
          <strong>{decision.question}</strong>
          <p className="subtle">{decision.context}</p>
          <textarea
            value={answers[decision.id] ?? ""}
            onChange={(event) => setAnswers({ ...answers, [decision.id]: event.target.value })}
          />
          <Action
            project={project}
            primary
            disabled={!answers[decision.id]?.trim()}
            onClick={() =>
              project.commands.answerDecision.run({
                id: decision.id,
                revision: decision.revision,
                answer: answers[decision.id]!.trim(),
              })
            }
          >
            Answer
          </Action>
        </div>
      ))}
    </Section>
  );
}
