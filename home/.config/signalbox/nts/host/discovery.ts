import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { relative, isAbsolute } from "node:path";
import { tmuxTerminal } from "signalbox/ext/integrations/tmux/terminal";
import type { NtsConfig } from "../config.ts";

const run = promisify(execFile);
export function projectPanes(output: string, roots: readonly string[]) {
  const seen = new Set<string>();
  return output.split("\n").flatMap((line) => {
    const [session, pane, path, command, title] = line.split("\t");
    if (!session || !pane || !path || !command || session.startsWith("sb_view_") || seen.has(pane))
      return [];
    if (!/(?:^|[/ -])(codex|claude)(?:$|[ -])/i.test(command)) return [];
    if (
      !roots.some((root) => {
        const child = relative(root, path);
        return child === "" || (!isAbsolute(child) && child !== ".." && !child.startsWith("../"));
      })
    )
      return [];
    seen.add(pane);
    return [
      {
        id: `tmux/${pane}`,
        label: title || command,
        path,
        target: tmuxTerminal({ target: session, pane }),
      },
    ];
  });
}
export async function discoverTerminals(config: NtsConfig, signal: AbortSignal) {
  if (!config.discoverTerminals) return [];
  try {
    const { stdout } = await run(
      "tmux",
      [
        "list-panes",
        "-a",
        "-F",
        "#{session_name}\t#{pane_id}\t#{pane_current_path}\t#{pane_current_command}\t#{pane_title}",
      ],
      {
        signal,
        timeout: 5000,
        maxBuffer: 1024 * 1024,
      },
    );
    return projectPanes(stdout, [config.root, config.poolRoot]);
  } catch (error) {
    const failure = error as NodeJS.ErrnoException & { stderr?: string };
    if (
      failure.code === "ENOENT" ||
      /no server running|no such file or directory|failed to connect/i.test(failure.stderr ?? "")
    )
      return [];
    throw error;
  }
}
