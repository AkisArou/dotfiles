// signalbox's global configuration: this machine, every project. `signalbox
// setup` linked the package here, so `import … from "signalbox"` has types.

import { claudeCode, codex, defaultActions, defineConfig, ghDash, machine, notify, type Action } from "signalbox";
import { nts } from "./nts/nts.ts";

/** lazygit in the selected agent's worktree. */
const lazygit: Action = {
  key: "l",
  label: "lazygit",
  when: (selection) => selection.kind === "agent",
  run: (selection, tools) => {
    if (selection.kind !== "agent") return;
    tools.terminal(["lazygit"], { cwd: selection.worktree?.path ?? selection.agent.cwd, title: `lazygit · ${selection.agent.name}` });
  },
};

export default defineConfig({
  // Every command signalbox runs, for any project, at low priority. CPUs 4–5 stay free by the systemd managers' own default.
  commandPrefix: ["nice", "-n", "19"],
  // The agents on this machine. In a project, only those working under its roots.
  providers: [codex(), claudeCode(), machine()],
  // Your actions on any view, and signalbox's own (e, o, f).
  // D: gh dash, on the selected GitHub issue or pull request when there is one.
  actions: [lazygit, ghDash(), ...defaultActions],
  // Every project's Events tab: errors also pop up on the desktop.
  events: { notify: [notify.desktop({ minLevel: "error" })] },
  // Each project's settings, by its folder: the one signalbox starts in is merged over the above.
  projects: {
    "~/Projects/nts": nts,
  },
});
