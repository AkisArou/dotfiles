// nts's web console: the kit's shell and standard pages, with nts's own pages
// (its status readings: Landing, Full check, Benchmarks, Conformance, Machine,
// Health) in the TUI's tab order, and what needs attention on Now.
import { createProjectRouter } from "@signalbox/kit/web/router";
import type { Page } from "@signalbox/kit/web/pages";
import {
  agentsPage,
  needsYouPage,
  nowPage,
  planPage,
  tasksPage,
} from "@signalbox/kit/web/standard-pages";
import { useProject } from "../shared/use-project.ts";
import type { Project } from "../shared/use-project.ts";
import type { NtsPage } from "../status/model.ts";
import { Attention } from "./views/attention.tsx";
import { NtsHealth, NtsView } from "./views/nts.tsx";

const status = (name: NtsPage, path: string): Page<Project> => ({
  name,
  path,
  render: (project) => <NtsView project={project} page={name} />,
});

export const router = createProjectRouter<Project>({
  name: "nts",
  useProject,
  readings: (project) => project.nts.readings,
  pages: [
    nowPage<Project>({ sections: (project) => <Attention project={project} /> }),
    needsYouPage,
    tasksPage,
    agentsPage,
    status("Landing", "/landing"),
    status("Full check", "/full-check"),
    status("Benchmarks", "/benchmarks"),
    status("Conformance", "/conformance"),
    planPage,
    status("Machine", "/machine"),
    { name: "Health", path: "/health", render: (project) => <NtsHealth project={project} /> },
  ],
});
