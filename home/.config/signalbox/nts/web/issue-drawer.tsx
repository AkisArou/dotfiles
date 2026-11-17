// An issue in a drawer, opened from any view by ?issue=<number>: its task first
// (where it is, what it waits for, the latest instruction, summary and design)
// with what you can do to it, then the issue itself, then its earlier tasks: as
// the TUI's Tasks detail shows them.
import { Drawer, DrawerHeader } from "@astryxdesign/core/Drawer";
import { Markdown } from "@astryxdesign/core/Markdown";
import { MetadataList, MetadataListItem } from "@astryxdesign/core/MetadataList";
import { Text } from "@astryxdesign/core/Text";
import type { Project } from "../shared/use-project.ts";
import { earlierTasks } from "../shared/actions.ts";
import {
  assignmentPhase,
  assignmentSays,
  assignmentTone,
  issueStateTerm,
  issueTone,
  liveTask,
  phaseLabel,
  taskMeaning,
} from "../shared/model.ts";
import { Status } from "./components.tsx";
import { TaskActions } from "./task-actions.tsx";

export function IssueDrawer({
  project,
  number,
  close,
}: {
  project: Project;
  number: number | undefined;
  close(): void;
}) {
  const issue = project.data.issues.find((candidate) => candidate.number === number);
  const task = number === undefined ? undefined : liveTask(project.data.assignments, number);
  const earlier = number === undefined ? [] : earlierTasks(project.data.assignments, number);
  const title = issue ? `#${issue.number} ${issue.title}` : `#${number}`;
  return (
    <Drawer
      isOpen={number !== undefined}
      onOpenChange={(open) => !open && close()}
      label={title}
      side="end"
      width={680}
      purpose="info"
    >
      {number !== undefined && (
        <>
          <DrawerHeader
            title={title}
            subtitle={
              issue && (
                <a href={issue.url} target="_blank" rel="noreferrer">
                  {issue.url}
                </a>
              )
            }
            onOpenChange={(open) => !open && close()}
          />
          <div className="drawer-body">
            {task && (
              <section className="drawer-section">
                <MetadataList columns={2}>
                  <MetadataListItem label="Task">
                    <Status tone={assignmentTone(task)} title={taskMeaning(task)}>
                      {`${assignmentPhase(task)} · round ${task.rounds + 1}`}
                    </Status>
                  </MetadataListItem>
                  <MetadataListItem label="Commit">
                    <span className="mono">{task.commit?.slice(0, 12) ?? "—"}</span>
                  </MetadataListItem>
                  <MetadataListItem label="Waiting for">{assignmentSays(task)}</MetadataListItem>
                  <MetadataListItem label="Tree">
                    <span className="mono">{task.tree ?? "—"}</span>
                  </MetadataListItem>
                </MetadataList>
                {task.note && (
                  <>
                    <Text type="label" weight="semibold">
                      Latest instruction
                    </Text>
                    <Markdown density="compact" headingLevelStart={4}>
                      {task.note}
                    </Markdown>
                  </>
                )}
                {task.summary && (
                  <>
                    <Text type="label" weight="semibold">
                      Summary
                    </Text>
                    <Markdown density="compact" headingLevelStart={4}>
                      {task.summary}
                    </Markdown>
                  </>
                )}
                {task.design && (
                  <>
                    <Text type="label" weight="semibold">
                      Design
                    </Text>
                    <Markdown density="compact" headingLevelStart={4}>
                      {task.design}
                    </Markdown>
                  </>
                )}
              </section>
            )}
            {issue && (
              <section className="drawer-section">
                {!task && (
                  <Text type="supporting" color="secondary">
                    No task runs for it.
                  </Text>
                )}
                <TaskActions project={project} issue={issue} task={task} />
              </section>
            )}
            {issue ? (
              <section className="drawer-section">
                <MetadataList columns={2}>
                  <MetadataListItem label="State">
                    <Status
                      tone={issueTone(issue.state)}
                      title={issueStateTerm(issue.state).meaning}
                    >
                      {issueStateTerm(issue.state).label}
                    </Status>
                  </MetadataListItem>
                  <MetadataListItem label="Owner">{issue.owner ?? "—"}</MetadataListItem>
                  <MetadataListItem label="Stream">{issue.stream}</MetadataListItem>
                  <MetadataListItem label="Group">{issue.group ?? "—"}</MetadataListItem>
                  {issue.parent && (
                    <MetadataListItem label="Parent">
                      #{issue.parent.number} {issue.parent.title}
                    </MetadataListItem>
                  )}
                  {issue.progress && issue.progress.total > 0 && (
                    <MetadataListItem label="Sub-issues">
                      {issue.progress.completed} of {issue.progress.total} complete
                    </MetadataListItem>
                  )}
                  {issue.blockedBy.length > 0 && (
                    <MetadataListItem label="Blocked by">
                      {issue.blockedBy.map((blocker) => `#${blocker}`).join(", ")}
                    </MetadataListItem>
                  )}
                  <MetadataListItem label="Labels">
                    <span className="secondary">{issue.labels.join(" · ") || "none"}</span>
                  </MetadataListItem>
                </MetadataList>
                <Markdown density="compact" headingLevelStart={3}>
                  {issue.body || "_No description._"}
                </Markdown>
              </section>
            ) : (
              <Text type="supporting" color="secondary">
                #{number} is not an open issue.
              </Text>
            )}
            {earlier.length > 0 && (
              <section className="drawer-section">
                <Text type="label" weight="semibold">
                  Earlier tasks
                </Text>
                {earlier.map((a) => (
                  <Text key={a.id} type="supporting" color="secondary">
                    {`${a.status} at ${phaseLabel(a.phase ?? a.status)} · round ${a.rounds + 1}${a.commit ? ` · ${a.commit.slice(0, 12)}` : ""}${a.error ? ` · ${a.error.split("\n", 1)[0]}` : ""}`}
                  </Text>
                ))}
              </section>
            )}
          </div>
        </>
      )}
    </Drawer>
  );
}
