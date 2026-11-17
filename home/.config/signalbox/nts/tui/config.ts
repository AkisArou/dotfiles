// USER CONFIGURATION: local tools and presentation policy, not SDK defaults.
import type { Issue } from "../shared/model.ts";
import type { Facet } from "./filters.ts";
import type { TerminalCommand } from "./tools.ts";

export const terminalConfig = {
  // Preserve FZF_DEFAULT_OPTS and FZF_DEFAULT_OPTS_FILE, including your colors.
  // One choice prints one candidate line. Given the lines already chosen (0-based),
  // several are chosen: those start ticked, and fzf prints them in the order chosen.
  finder: (title: string, chosen?: readonly number[]): TerminalCommand => ({
    command: "fzf",
    args: [
      ...(chosen ? ["--multi"] : ["--no-multi"]),
      "--no-print-query",
      "--no-expect",
      "--prompt",
      `${title}> `,
      ...(chosen
        ? [
            "--header",
            "Tab ticks or unticks; Enter keeps the ticked ones, in the order ticked",
            ...(chosen.length
              ? [
                  "--bind",
                  // load, not start: fzf reads its input in the background, and the
                  // lines to tick exist only once it has all of them.
                  `load:${chosen.map((index) => `pos(${index + 1})+select`).join("+")}+first`,
                ]
              : []),
          ]
        : []),
    ],
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
