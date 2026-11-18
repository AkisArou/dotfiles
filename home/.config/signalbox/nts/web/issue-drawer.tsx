// An issue in a drawer, opened from any view by ?issue=<number>: what you can do
// to its task first, then where the task is and what it waits for, then the issue
// itself, then the task's long notes (its latest instruction, summary and design)
// and its earlier tasks: the short and actionable first, as the TUI's Tasks detail.
import type { ReactNode } from "react";
import { Drawer, DrawerHeader } from "@astryxdesign/core/Drawer";
import { Icon } from "@astryxdesign/core/Icon";
import { IconButton } from "@astryxdesign/core/IconButton";
import { Markdown } from "@astryxdesign/core/Markdown";
import { MetadataList, MetadataListItem } from "@astryxdesign/core/MetadataList";
import { Text } from "@astryxdesign/core/Text";
import type { Project } from "../shared/use-project.ts";
import { current } from "../shared/use-project.ts";
import { earlierTasks } from "@signalbox/kit/shared/actions";
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
import { useNarrow } from "./narrow.ts";
import { useSlideBack } from "./slide.ts";

export function IssueDrawer({
  project,
  number,
  close,
}: {
  project: Project;
  number: number | undefined;
  close(): void;
}) {
  const narrow = useNarrow();
  const slide = useSlideBack(close, number);
  const issue = project.data.issues.find((candidate) => candidate.number === number);
  const task = number === undefined ? undefined : liveTask(project.data.assignments, number);
  const earlier = number === undefined ? [] : earlierTasks(project.data.assignments, number);
  const title = issue ? `#${issue.number} ${issue.title}` : `#${number}`;
  const drawer = (content: ReactNode) => (
    <Drawer
      isOpen={number !== undefined}
      onOpenChange={(open) => !open && close()}
      label={title}
      side="end"
      width={680}
      purpose="info"
    >
      {content && (
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
          {content}
        </>
      )}
    </Drawer>
  );
  if (number === undefined) return narrow ? null : drawer(null);
  // Actions, task, issue, notes and earlier tasks: one body for both containers.
  const body = (
    <div className="drawer-body">
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
      {task && (
        <section className="drawer-section">
          <MetadataList columns={narrow ? 1 : 2}>
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
        </section>
      )}
      {issue ? (
        <section className="drawer-section">
          <MetadataList columns={narrow ? 1 : 2}>
            <MetadataListItem label="State">
              <Status tone={issueTone(issue.state)} title={issueStateTerm(issue.state).meaning}>
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
          {/* Until the issues arrive, a link to one is not yet known to be closed. */}
          {current(project.issues) === undefined
            ? `Loading #${number}…`
            : `#${number} is not an open issue.`}
        </Text>
      )}
      {task && (task.note || task.summary || task.design) && (
        <section className="drawer-section">
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
  );
  // A phone: a full-screen page, as the agent view is; a desktop: the drawer.
  return narrow ? (
    <div
      className={slide.leaving ? "issue-screen leaving" : "issue-screen"}
      role="dialog"
      aria-label={title}
      onAnimationEnd={slide.onAnimationEnd}
    >
      <header className="agent-screen-head">
        <IconButton
          label="Back"
          variant="ghost"
          size="sm"
          onClick={slide.back}
          icon={<Icon icon="chevronLeft" />}
        />
        <div className="agent-screen-title">
          <Text type="label" weight="semibold" className="issue-screen-title">
            {title}
          </Text>
          {issue && (
            <a className="issue-screen-link" href={issue.url} target="_blank" rel="noreferrer">
              Open on GitHub
            </a>
          )}
        </div>
      </header>
      <div className="issue-screen-body">{body}</div>
    </div>
  ) : (
    drawer(body)
  );
}
