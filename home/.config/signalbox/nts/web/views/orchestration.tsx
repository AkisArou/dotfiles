// The orchestration controls: which profile each role launches with, which
// harnesses are available, the task assignments, and starting one by hand.
import { useState } from "react";
import type { Project } from "../../shared/use-project.ts";
import type { AgentRole, Harness } from "../../shared/model.ts";
import { assignmentPhase, assignmentSays } from "../../shared/model.ts";
import { Action, Badge, Empty, Section } from "../components.tsx";

const roleNames: Record<AgentRole, string> = {
  coordinator: "Coordinator",
  worker: "Workers",
  reviewer: "Reviewers",
};

export function Orchestration({ project }: { project: Project }) {
  const settings = project.data.profiles;
  const [issue, setIssue] = useState("");
  const [instructions, setInstructions] = useState("");
  if (!settings) return <Empty>Orchestration settings are loading.</Empty>;
  const change = (role: AgentRole, field: "profile" | "fallback", value: string) => {
    const current = settings.roles[role];
    const next = { ...current, [field]: value || undefined };
    return project.perform(() =>
      project.commands.setRole.run({
        role,
        profile: next.profile,
        ...(next.fallback ? { fallback: next.fallback } : {}),
        revision: settings.revision,
      }),
    );
  };
  return (
    <div className="orchestration">
      <Section title="Profiles" aside={<span className="subtle">APPLIES TO THE NEXT LAUNCH</span>}>
        <table className="table">
          <thead>
            <tr>
              <th>Role</th>
              <th>Profile</th>
              <th>Fallback</th>
            </tr>
          </thead>
          <tbody>
            {(Object.keys(roleNames) as AgentRole[]).map((role) => (
              <tr key={role}>
                <td>{roleNames[role]}</td>
                <td>
                  <select
                    value={settings.roles[role].profile}
                    disabled={project.busy}
                    onChange={(event) => void change(role, "profile", event.target.value)}
                  >
                    {settings.profiles.map((profile) => (
                      <option key={profile.id} value={profile.id}>
                        {profile.label}
                      </option>
                    ))}
                  </select>
                </td>
                <td>
                  <select
                    value={settings.roles[role].fallback ?? ""}
                    disabled={project.busy}
                    onChange={(event) => void change(role, "fallback", event.target.value)}
                  >
                    <option value="">None (wait)</option>
                    {settings.profiles.map((profile) => (
                      <option key={profile.id} value={profile.id}>
                        {profile.label}
                      </option>
                    ))}
                  </select>
                </td>
              </tr>
            ))}
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
          <Action
            project={project}
            primary
            disabled={!issue}
            onClick={async () => {
              await project.commands.assignTask.run({ issue: Number(issue), instructions });
              setIssue("");
              setInstructions("");
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
