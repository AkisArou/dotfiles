// Read-only prerequisites; no agents, builds in NTS, services or worktrees are started.
import { stat } from "node:fs/promises";
import { join } from "node:path";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { ntsConfig as config } from "../config.ts";
const run = promisify(execFile);
let failures = 0;
const check = async (name: string, required: boolean, action: () => Promise<unknown>) => {
  try {
    await action();
    console.log(`✓ ${name}`);
  } catch (error) {
    if (required) failures++;
    console.log(
      `${required ? "✗" : "·"} ${name}: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};
console.log(`NTS · ${config.root}\nGitHub · ${config.repo}\nState · ${config.state}\n`);
await Promise.all([
  check("NTS checkout and status tools", true, async () => {
    for (const file of [
      ".git",
      config.plan,
      "tooling/records/summary.ts",
      "tooling/landing/check.ts",
      "tooling/landing/pool.ts",
      "tooling/full-check/service.ts",
      "tooling/gate/admit.ts",
    ])
      await stat(join(config.root, file));
  }),
  check("GitHub access (gh login or GH_TOKEN)", true, () =>
    run("gh", ["repo", "view", config.repo, "--json", "nameWithOwner"], { timeout: 20_000 }),
  ),
  check("tmux (persistent native terminals)", true, () => run("tmux", ["-V"])),
  check("fzf (TUI finder and filters)", false, () => run("fzf", ["--version"])),
  check("lazygit (TUI Git view)", false, () => run("lazygit", ["--version"])),
  ...config.terminalPrograms
    .filter((program) => program.id !== "shell")
    .map((program) =>
      check(`${program.label} CLI`, false, () =>
        run(program.command, ["--version"], { timeout: 10_000 }),
      ),
    ),
]);
process.exitCode = failures ? 1 : 0;
