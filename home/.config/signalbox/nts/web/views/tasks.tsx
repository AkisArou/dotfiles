// Tasks: every open task in one line each, found by text and by the same
// facets as the TUI (several values at once: OR within a facet, AND between
// facets), kept in the URL so a filtered list can be linked; a task opens in
// the issue drawer. A task the coordinator queued shows where it is.
import { useNavigate, useSearch } from "@tanstack/react-router";
import { List, ListItem } from "@astryxdesign/core/List";
import { MultiSelector } from "@astryxdesign/core/MultiSelector";
import { Table, pixel, proportional } from "@astryxdesign/core/Table";
import { TextInput } from "@astryxdesign/core/TextInput";
import { Toolbar } from "@astryxdesign/core/Toolbar";
import {
  chosenOf,
  describeTaskFilter,
  filterKey,
  filterOptions,
  keysOf,
  matches,
  taskFacets,
} from "../../shared/filters.ts";
import {
  assignmentTone,
  issueStateTerm,
  issueTone,
  liveTask,
  taskMeaning,
  taskWord,
} from "../../shared/model.ts";
import type { Project } from "../../shared/use-project.ts";
import { Empty, Section, Status } from "../components.tsx";
import { useNarrow } from "../narrow.ts";

type Search = { q?: string; filter?: string[]; issue?: number };

export function Tasks({ project }: { project: Project }) {
  const navigate = useNavigate();
  const narrow = useNarrow();
  const { q = "", filter = [] } = useSearch({ strict: false }) as Search;
  const search = (change: Partial<Search>) =>
    void navigate({
      to: "/tasks",
      search: (previous: Search) => {
        const next: Search = { ...previous, ...change };
        if (!next.q) delete next.q;
        if (!next.filter?.length) delete next.filter;
        return next;
      },
      replace: true,
    });
  const issues = project.data.issues;
  const chosen = chosenOf(filter);
  const visible = issues.filter(
    (issue) =>
      matches(taskFacets, chosen, issue) &&
      `${issue.number} ${issue.title} ${issue.labels.join(" ")} ${issue.owner ?? ""}`
        .toLowerCase()
        .includes(q.toLowerCase()),
  );
  const options = filterOptions(
    taskFacets,
    chosen,
    issues,
    describeTaskFilter(project.data.labels),
  ).map((option) => ({
    value: filterKey(option.facet, option.value),
    label: `${option.label} (${option.count})`,
    ...(option.meaning ? { description: option.meaning } : {}),
  }));
  const rows = visible.map((issue) => {
    const live = liveTask(project.data.assignments, issue.number);
    return {
      id: issue.id,
      number: issue.number,
      title: issue.title,
      tone: live ? assignmentTone(live) : issueTone(issue.state),
      state: live ? taskWord(live) : issueStateTerm(issue.state).label,
      meaning: live ? taskMeaning(live) : issueStateTerm(issue.state).meaning,
      place: issue.group ?? issue.stream,
      owner: issue.owner ?? "",
    };
  });
  // The search and the filters: one row on a desktop, stacked on a phone.
  const controls = (
    <>
      <TextInput
        label="Find a task"
        isLabelHidden
        id="task-search"
        type="search"
        size="sm"
        width={narrow ? "100%" : 340}
        placeholder="Find by number, title, label or owner  (/)"
        value={q}
        onChange={(value) => search({ q: value })}
      />
      <MultiSelector
        label="Filters"
        isLabelHidden
        size="sm"
        width={narrow ? "100%" : 320}
        placeholder="Filter by area, stream, kind, state, label…"
        options={options}
        value={keysOf(chosen)}
        onChange={(value) => search({ filter: value })}
        hasSearch
        searchPlaceholder="Find a filter"
        hasClear
        triggerDisplay="badges"
        maxBadges={3}
      />
    </>
  );
  return (
    <Section
      title="Tasks"
      count={
        visible.length === issues.length ? issues.length : `${visible.length} of ${issues.length}`
      }
    >
      {narrow ? (
        <div className="panel-body stack">{controls}</div>
      ) : (
        <Toolbar label="Find tasks" startContent={controls} />
      )}
      {rows.length && narrow ? (
        <List density="compact" hasDividers edge="padded">
          {rows.map((row) => (
            <ListItem
              key={row.id}
              label={row.title}
              description={`#${row.number} · ${row.place}`}
              endContent={
                <Status tone={row.tone} title={row.meaning}>
                  {row.state}
                </Status>
              }
              onClick={() => search({ issue: row.number })}
            />
          ))}
        </List>
      ) : rows.length ? (
        <Table
          data={rows}
          idKey="id"
          density="compact"
          dividers="rows"
          hasHover
          textOverflow="truncate"
          columns={[
            {
              key: "number",
              header: "#",
              width: pixel(64),
              renderCell: (row) => <span className="mono secondary">{row.number}</span>,
            },
            {
              key: "title",
              header: "Title",
              width: proportional(4),
              renderCell: (row) => (
                <button
                  type="button"
                  className="cell-link"
                  title={row.title}
                  onClick={() => search({ issue: row.number })}
                >
                  {row.title}
                </button>
              ),
            },
            {
              key: "state",
              header: "State",
              width: pixel(170),
              renderCell: (row) => (
                <Status tone={row.tone} title={row.meaning}>
                  {row.state}
                </Status>
              ),
            },
            {
              key: "place",
              header: "Group",
              width: proportional(2),
              renderCell: (row) => (
                <span className="secondary" title={row.place}>
                  {row.place}
                </span>
              ),
            },
            { key: "owner", header: "Owner", width: pixel(140) },
          ]}
        />
      ) : (
        <Empty>No matching task</Empty>
      )}
    </Section>
  );
}
