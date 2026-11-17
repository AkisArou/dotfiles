// Tasks: every open task in one line each, found by text, stream and state
// (kept in the URL, so a filtered list can be linked); a task opens in the
// issue drawer.
import { issueTone } from "../../shared/model.ts";
import { useNavigate, useSearch } from "@tanstack/react-router";
import { Selector } from "@astryxdesign/core/Selector";
import { Table, pixel, proportional } from "@astryxdesign/core/Table";
import { TextInput } from "@astryxdesign/core/TextInput";
import { Toolbar } from "@astryxdesign/core/Toolbar";
import type { Project } from "../../shared/use-project.ts";
import { Empty, Section, Status } from "../components.tsx";

type Filters = { q?: string; stream?: string; state?: string; issue?: number };

export function Tasks({ project }: { project: Project }) {
  const navigate = useNavigate();
  const { q = "", stream = "all", state = "all" } = useSearch({ strict: false }) as Filters;
  const filter = (change: Partial<Filters>) =>
    void navigate({
      to: "/tasks",
      search: (previous: Filters) => {
        const next: Filters = { ...previous, ...change };
        for (const key of ["q", "stream", "state"] as const)
          if (!next[key] || next[key] === "all") delete next[key];
        return next;
      },
      replace: true,
    });
  const issues = project.data.issues;
  const visible = issues.filter(
    (issue) =>
      (stream === "all" || issue.stream === stream) &&
      (state === "all" || issue.state === state) &&
      `${issue.number} ${issue.title} ${issue.labels.join(" ")} ${issue.owner ?? ""}`
        .toLowerCase()
        .includes(q.toLowerCase()),
  );
  const rows = visible.map((issue) => ({
    id: issue.id,
    number: issue.number,
    title: issue.title,
    state: issue.state,
    place: issue.group ?? issue.stream,
    owner: issue.owner ?? "",
  }));
  return (
    <Section
      title="Tasks"
      count={
        visible.length === issues.length ? issues.length : `${visible.length} of ${issues.length}`
      }
    >
      <Toolbar
        label="Find tasks"
        startContent={
          <>
            <TextInput
              label="Find a task"
              isLabelHidden
              id="task-search"
              type="search"
              size="sm"
              placeholder="Find by number, title, label or owner  (/)"
              value={q}
              onChange={(value) => filter({ q: value })}
            />
            <Selector
              label="Stream"
              isLabelHidden
              value={stream}
              onChange={(value) => filter({ stream: value })}
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
              onChange={(value) => filter({ state: value })}
              options={[
                { value: "all", label: "All states" },
                ...[...new Set(issues.map((issue) => issue.state))].map((value) => ({
                  value,
                  label: value,
                })),
              ]}
            />
          </>
        }
      />
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
                  onClick={() => filter({ issue: row.number })}
                >
                  {row.title}
                </button>
              ),
            },
            {
              key: "state",
              header: "State",
              width: pixel(130),
              renderCell: (row) => <Status tone={issueTone(row.state)}>{row.state}</Status>,
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
