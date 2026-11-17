// USER CONFIGURATION: local tools and presentation policy, not SDK defaults.
import type { Issue } from "../shared/model.ts";
import type { Facet } from "./filters.ts";
import type { TerminalCommand } from "./tools.ts";

export const terminalConfig = {
  // Preserve FZF_DEFAULT_OPTS and FZF_DEFAULT_OPTS_FILE, including your colors.
  // These flags keep selection output to a single candidate line.
  finder: (title: string): TerminalCommand => ({
    command: "fzf",
    args: ["--no-multi", "--no-print-query", "--no-expect", "--prompt", `${title}> `],
  }),
  git: { command: "lazygit" } satisfies TerminalCommand,
  // Opens settings.json from the Agents tab's Settings file row.
  editor: { command: process.env.VISUAL ?? process.env.EDITOR ?? "vi" } satisfies TerminalCommand,
  taskFacets: [
    { id: "area", label: "area", values: (issue) => [issue.area] },
    { id: "stream", label: "stream", values: (issue) => [issue.stream] },
    {
      id: "kind",
      label: "kind",
      values: (issue) =>
        issue.labels.filter((label) => label.startsWith("kind:")).map((label) => label.slice(5)),
    },
    { id: "state", label: "state", values: (issue) => [issue.state] },
    {
      id: "label",
      label: "label",
      values: (issue) => issue.labels.filter((label) => !label.startsWith("issue:")),
    },
    {
      id: "assigned",
      label: "assigned",
      values: (issue) => [issue.owner ? "yes" : "no"],
    },
  ] satisfies readonly Facet<Issue>[],
};
