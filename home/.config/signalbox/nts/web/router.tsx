// The console's routes, defined in code: one per view, the same views and order
// as the TUI's tabs, with Agents' three panes as child routes. ?issue=<n> on any
// route opens that issue in the drawer; Tasks keeps its filters in its search.
import type { ReactNode } from "react";
import { createRootRoute, createRoute, createRouter, Outlet } from "@tanstack/react-router";
import type { Project } from "../shared/use-project.ts";
import { Shell } from "./app.tsx";
import { useProjectContext } from "./project.ts";
import { Now } from "./views/now.tsx";
import { NeedsYou } from "./views/needs.tsx";
import { Tasks } from "./views/tasks.tsx";
import { AgentPanes, Sessions } from "./views/agents.tsx";
import { Settings } from "./views/orchestration.tsx";
import { Plan } from "./views/plan.tsx";
import { NtsHealth, NtsView } from "./views/nts.tsx";

const text = (value: unknown) => (typeof value === "string" && value ? value : undefined);

const rootRoute = createRootRoute({
  component: Shell,
  // Over any view: an issue in its drawer (?issue=n), an agent full screen (?screen=id).
  validateSearch: (search: Record<string, unknown>): { issue?: number; screen?: string } => {
    const issue = Number(search.issue);
    return {
      ...(Number.isInteger(issue) && issue > 0 ? { issue } : {}),
      ...(typeof search.screen === "string" && search.screen ? { screen: search.screen } : {}),
    };
  },
});

/** A view's route: its component gets the project the shell reads. */
const view = (path: string, render: (project: Project) => ReactNode) =>
  createRoute({
    getParentRoute: () => rootRoute,
    path,
    component: () => render(useProjectContext()),
  });

const tasksRoute = createRoute({
  getParentRoute: () => rootRoute,
  path: "/tasks",
  // The filters as facet:value keys (see @signalbox/kit/shared/filters), several at once.
  validateSearch: (search: Record<string, unknown>): { q?: string; filter?: string[] } => {
    const filter = (Array.isArray(search.filter) ? search.filter : [search.filter]).filter(
      (key): key is string => typeof key === "string" && key.length > 0,
    );
    return {
      ...(text(search.q) ? { q: text(search.q) } : {}),
      ...(filter.length ? { filter } : {}),
    };
  },
  component: () => <Tasks project={useProjectContext()} />,
});

const agentsRoute = createRoute({
  getParentRoute: () => rootRoute,
  path: "/agents",
  component: () => (
    <AgentPanes>
      <Outlet />
    </AgentPanes>
  ),
});
const pane = (path: string, render: (project: Project) => ReactNode) =>
  createRoute({
    getParentRoute: () => agentsRoute,
    path,
    component: () => render(useProjectContext()),
  });

const routeTree = rootRoute.addChildren([
  view("/", (project) => <Now project={project} />),
  view("/needs-you", (project) => <NeedsYou project={project} />),
  tasksRoute,
  agentsRoute.addChildren([
    pane("/", (project) => <Sessions project={project} />),
    pane("/settings", (project) => <Settings project={project} />),
  ]),
  view("/landing", (project) => <NtsView project={project} page="Landing" />),
  view("/full-check", (project) => <NtsView project={project} page="Full check" />),
  view("/benchmarks", (project) => <NtsView project={project} page="Benchmarks" />),
  view("/conformance", (project) => <NtsView project={project} page="Conformance" />),
  view("/plan", (project) => <Plan project={project} />),
  view("/machine", (project) => <NtsView project={project} page="Machine" />),
  view("/health", (project) => <NtsHealth project={project} />),
]);

// Each view keeps its own scroll: the router records the view's scroller (app.tsx,
// data-scroll-restoration-id="view") per path, restores it on returning to that view
// (a tab, or Back), and starts a view not seen yet at its top. An issue or an agent
// opened over a view changes only the search, so the view stays where it was.
export const router = createRouter({
  routeTree,
  defaultPreload: false,
  scrollRestoration: true,
  getScrollRestorationKey: (location) => location.pathname,
  scrollToTopSelectors: ['[data-scroll-restoration-id="view"]'],
});

declare module "@tanstack/react-router" {
  interface Register {
    router: typeof router;
  }
}
