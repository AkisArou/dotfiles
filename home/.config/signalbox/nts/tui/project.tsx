/** @jsxImportSource @opentui/react */
// nts's terminal app: the kit's pages, with nts's status readings (Landing, Full
// check, Benchmarks, Conformance, Machine, Health) in the web console's order, and
// what needs attention on Now.
import {
  agentsTuiPage,
  needsYouTuiPage,
  nowTuiPage,
  planTuiPage,
  tasksTuiPage,
} from "@signalbox/kit/tui/pages";
import type { TuiApp, TuiPage } from "@signalbox/kit/tui/pages";
import { useProject } from "../shared/use-project.ts";
import type { Project } from "../shared/use-project.ts";
import { cellText, tableRows } from "../status/model.ts";
import type { NtsPage } from "../status/model.ts";
import { Attention } from "./attention.tsx";
import { NtsContent } from "./nts.tsx";

/** A status page: its tables' rows to step through, and the page with the selected one. */
const status = (name: NtsPage | "Health"): TuiPage<Project> => ({
  name,
  kind: "document",
  rows: (project) =>
    tableRows(name === "Health" ? project.nts.health : project.nts.page(name)).map((item) => ({
      id: item.id,
      line: `${item.table.title} · ${item.table.columns.map((column) => cellText(item.row[column.key])).join(" · ")}`,
    })),
  render: (project, view) => (
    <NtsContent
      project={project}
      page={name}
      width={view.width}
      {...(view.selected ? { selected: view.selected } : {})}
      details={view.details}
      onSelect={view.select}
    />
  ),
});

export const app: TuiApp<Project> = {
  name: "nts",
  useProject,
  readings: (project) => project.nts.readings,
  pages: [
    nowTuiPage<Project>({ sections: (project) => <Attention project={project} /> }),
    needsYouTuiPage,
    tasksTuiPage,
    agentsTuiPage,
    status("Landing"),
    status("Full check"),
    status("Benchmarks"),
    status("Conformance"),
    planTuiPage,
    status("Machine"),
    status("Health"),
  ],
};
