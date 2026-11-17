// The project's live data for every route, read once by the shell and shared.
import { createContext, use } from "react";
import type { Project } from "../shared/use-project.ts";
import type { Tab as View } from "../shared/model.ts";

export const ProjectContext = createContext<Project | null>(null);

export function useProjectContext(): Project {
  const project = use(ProjectContext);
  if (!project) throw new Error("The project is read by the shell, above every route");
  return project;
}

/** Each view's path; the same views, in the same order, as the TUI's tabs. */
export const viewPath: Record<View, string> = {
  Now: "/",
  "Needs you": "/needs-you",
  Tasks: "/tasks",
  Agents: "/agents",
  Landing: "/landing",
  "Full check": "/full-check",
  Benchmarks: "/benchmarks",
  Conformance: "/conformance",
  Plan: "/plan",
  Machine: "/machine",
  Health: "/health",
};

/** The view a path belongs to (a sub-path such as /agents/settings belongs to Agents). */
export function viewOf(pathname: string): View {
  const entries = Object.entries(viewPath) as [View, string][];
  return (
    entries.find(
      ([, path]) => path !== "/" && (pathname === path || pathname.startsWith(`${path}/`)),
    )?.[0] ?? "Now"
  );
}
