import { useState } from "react";
import { Button } from "@astryxdesign/core/Button";
import { TextInput } from "@astryxdesign/core/TextInput";
import { Selector } from "@astryxdesign/core/Selector";
import type { Project } from "../../shared/use-project.ts";
import { Badge, Empty } from "../components.tsx";

export function Tasks({ project, needsYou = false }: { project: Project; needsYou?: boolean }) {
  const [query, setQuery] = useState(""),
    [stream, setStream] = useState("all"),
    [state, setState] = useState("all"),
    [selected, setSelected] = useState<string>();
  const issues = project.data.issues.filter((issue) =>
    needsYou ? issue.state === "needs-user" : issue.state !== "needs-user",
  );
  const visible = issues.filter(
    (issue) =>
      (stream === "all" || issue.stream === stream) &&
      (state === "all" || issue.state === state) &&
      `${issue.number} ${issue.title} ${issue.labels.join(" ")} ${issue.owner ?? ""}`
        .toLowerCase()
        .includes(query.toLowerCase()),
  );
  const task = visible.find((issue) => issue.id === selected);
  return (
    <div className={`detail-layout ${task ? "has-detail" : ""}`}>
      <div>
        <div className="toolbar">
          <TextInput
            label="Search tasks"
            isLabelHidden
            id="task-search"
            type="search"
            placeholder="Find an issue or label…"
            value={query}
            onChange={setQuery}
            containerClassName="search-field"
            size="lg"
          />
          <Selector
            label="Stream"
            isLabelHidden
            value={stream}
            onChange={setStream}
            options={[
              { value: "all", label: "All streams" },
              ...[...new Set(issues.map((issue) => issue.stream))].map((value) => ({
                value,
                label: value,
              })),
            ]}
          />
          <Selector
            label="State"
            isLabelHidden
            value={state}
            onChange={setState}
            options={[
              { value: "all", label: "All states" },
              ...[...new Set(issues.map((issue) => issue.state))].map((value) => ({
                value,
                label: value,
              })),
            ]}
          />
          <span className="subtle">{visible.length} issues</span>
        </div>
        <div className="table-scroll">
          <table>
            <thead>
              <tr>
                <th>Issue</th>
                <th>Stream / group</th>
                <th>State</th>
                <th>Owner</th>
              </tr>
            </thead>
            <tbody>
              {visible.map((issue) => (
                <tr key={issue.id} className={issue.id === selected ? "selected-row" : ""}>
                  <td>
                    <Button
                      label={`#${issue.number} ${issue.title}`}
                      variant="ghost"
                      className="issue-link"
                      onClick={() => setSelected(issue.id)}
                    >
                      <span className="issue-link-content">
                        <span className="issue-number">#{issue.number}</span>
                        <span>{issue.title}</span>
                      </span>
                    </Button>
                  </td>
                  <td>
                    {issue.stream}
                    {issue.group && <span className="subtle"> › {issue.group}</span>}
                  </td>
                  <td>
                    <Badge state={issue.state}>{issue.state}</Badge>
                  </td>
                  <td>{issue.owner ?? "—"}</td>
                </tr>
              ))}
            </tbody>
          </table>
          {!visible.length && <Empty>No matching issues.</Empty>}
        </div>
      </div>
      {task && (
        <aside className="detail-panel">
          <div className="section-heading">
            <a href={task.url} target="_blank" rel="noreferrer">
              GitHub #{task.number} ↗
            </a>
            <Button
              label="Close issue details"
              icon={<span>×</span>}
              isIconOnly
              variant="ghost"
              onClick={() => setSelected(undefined)}
            />
          </div>
          <h2>{task.title}</h2>
          {task.workspace && <p className="mono">{task.workspace}</p>}
          {task.claims?.map((claim) => (
            <p key={`${claim.what}/${claim.path}`}>{claim.what}</p>
          ))}
          <Badge state={task.state}>{task.state}</Badge>
          <div className="label-list">
            {task.labels.map((label) => (
              <span key={label}>{label}</span>
            ))}
          </div>
          {task.parent && (
            <p>
              Parent: #{task.parent.number} {task.parent.title}
            </p>
          )}
          {task.progress && (
            <p>
              {task.progress.completed} / {task.progress.total} sub-issues complete
            </p>
          )}
          {task.blockedBy.length > 0 && (
            <p>Blocked by {task.blockedBy.map((number) => `#${number}`).join(", ")}</p>
          )}
          <pre className="detail-body">{task.body}</pre>
        </aside>
      )}
    </div>
  );
}
