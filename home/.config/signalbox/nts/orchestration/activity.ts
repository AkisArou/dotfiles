// What each Claude session is doing, from `claude agents --json`: busy while a
// turn runs, idle while it waits. The SDK's "running" only means the process is
// alive, which an idle agent waiting for its next message also is.
import { execFile } from "node:child_process";
import { capability, Cell, provide } from "signalbox/core";
import type { Extension, Readable } from "signalbox/core";
import { poll } from "signalbox/core/utils";

/** Session ID to Claude Code's own status ("busy", "idle", ...). */
export type ClaudeActivity = Readonly<Record<string, string>>;

export function parseClaudeAgents(stdout: string): ClaudeActivity {
  const listed = JSON.parse(stdout) as unknown;
  const sessions = Array.isArray(listed)
    ? listed
    : ((listed as { sessions?: unknown[]; agents?: unknown[] }).sessions ??
      (listed as { agents?: unknown[] }).agents ??
      []);
  const activity: Record<string, string> = {};
  for (const session of sessions as { sessionId?: unknown; session_id?: unknown; status?: unknown }[]) {
    const id = session.sessionId ?? session.session_id;
    if (typeof id === "string" && typeof session.status === "string") activity[id] = session.status;
  }
  return activity;
}

export function claudeActivity(options: { readonly interval?: number } = {}) {
  const key = capability<Readable<ClaudeActivity>>("nts/claude-activity");
  const cell = new Cell<ClaudeActivity>({});
  const extension: Extension = {
    id: "nts-claude-activity",
    provides: [provide(key, cell)],
    start(context) {
      const read = () =>
        new Promise<ClaudeActivity>((resolve, reject) =>
          execFile("claude", ["agents", "--json"], { timeout: 15_000, maxBuffer: 4 * 1024 * 1024, signal: context.signal }, (error, stdout) =>
            error ? reject(error) : resolve(parseClaudeAgents(stdout)),
          ),
        );
      context.afterStart(() => {
        // Each read starts the Claude CLI, so it runs every 15 s, not continuously.
        // A failed read keeps the last reading: activity decorates the agents list
        // and must never take it down.
        const loop = poll(async () => cell.set(await read()), options.interval ?? 15_000, context.signal, (error) =>
          context.log.error("claude activity", error),
        );
        context.onStop(() => loop);
      });
    },
  };
  return { key, extension };
}

/** A managed Claude instance's ID is "claude/<session id>". */
export function sessionOf(instanceId: string): string | undefined {
  const match = /^claude\/([0-9a-f-]{36})$/.exec(instanceId);
  return match?.[1];
}
