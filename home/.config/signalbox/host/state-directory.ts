// NTS application state is independent of where its source package lives.
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { expandPath, hash } from "signalbox/core/utils";

export function ntsStateDirectory(options: {
  appDirectory: string;
  projectRoot: string;
  override?: string;
  stateHome?: string;
}): string {
  if (options.override !== undefined) return resolve(expandPath(options.override));
  // Existing executors write receipts here and the tmux socket identity is derived
  // from this path. Keep it in place; moving a live database would split ownership.
  const previous = resolve(options.appDirectory, "../v2/playground/.signalbox/nts");
  if (
    existsSync(join(previous, "runtime.sqlite")) ||
    existsSync(join(previous, "host-lock.sqlite"))
  )
    return previous;
  const home = resolve(
    expandPath(options.stateHome ?? process.env.XDG_STATE_HOME ?? join(homedir(), ".local/state")),
  );
  return join(
    home,
    "signalbox",
    "nts",
    hash(resolve(expandPath(options.projectRoot))).slice(0, 12),
  );
}
