// What each agent is doing: busy while a turn runs, idle while it waits. The
// SDK's "running" only means the process is alive, which an idle agent waiting
// for its next message also is. Claude Code reports it (`claude agents --json`);
// Codex has no such reading, so its agents stay unknown.
import { execFile } from "node:child_process";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import { poll } from "signalbox/core/utils";

/** Agent instance ID ("claude/<session>") to its activity ("busy", "idle", ...). */
export type AgentActivity = Readonly<Record<string, string>>;

export function parseClaudeAgents(stdout: string): AgentActivity {
  const listed = JSON.parse(stdout) as unknown;
  const sessions = Array.isArray(listed)
    ? listed
    : ((listed as { sessions?: unknown[] }).sessions ?? (listed as { agents?: unknown[] }).agents ?? []);
  const activity: Record<string, string> = {};
  for (const session of sessions as { sessionId?: unknown; session_id?: unknown; status?: unknown }[]) {
    const id = session.sessionId ?? session.session_id;
    if (typeof id === "string" && typeof session.status === "string") activity[`claude/${id}`] = session.status;
  }
  return activity;
}

/** A readable activity source; `read` (if any) refreshes it on an interval. */
export function activitySource(options: {
  readonly id: string;
  readonly cell?: Cell<AgentActivity>;
  readonly read?: (signal: AbortSignal) => Promise<AgentActivity>;
  readonly interval?: number;
}): { readonly key: Capability<Readable<AgentActivity>>; readonly extension: Extension } {
  const key = capability<Readable<AgentActivity>>(`nts/activity/${options.id}`);
  const cell = options.cell ?? new Cell<AgentActivity>({});
  const read = options.read;
  return {
    key,
    extension: {
      id: `nts-activity-${options.id}`,
      provides: [provide(key, cell)],
      start(context) {
        if (!read) return;
        let last = "";
        context.afterStart(() => {
          // A failed read keeps the last reading: activity decorates and never decides alone.
          const loop = poll(
            async () => {
              const next = await read(context.signal);
              const text = JSON.stringify(next);
              // Only a change wakes the routes that watch it.
              if (text !== last) (last = text), cell.set(next);
            },
            options.interval ?? 15_000,
            context.signal,
            (error) => context.log.error("agent activity", error),
          );
          context.onStop(() => loop);
        });
      },
    },
  };
}

/** Claude Code's sessions, read every 15 s (each read starts the CLI). */
export function claudeActivity() {
  return activitySource({
    id: "claude",
    read: (signal) =>
      new Promise((resolve, reject) =>
        execFile(
          "claude",
          ["agents", "--json"],
          { timeout: 15_000, maxBuffer: 4 * 1024 * 1024, signal },
          (error, stdout) => (error ? reject(error) : resolve(parseClaudeAgents(stdout))),
        ),
      ),
  });
}
