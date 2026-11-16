// NTS application state is independent of where its source package lives: one
// directory per project root, so moving the application keeps its state, its
// execution receipts and the tmux socket identities derived from them.
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { expandPath, hash } from "signalbox/core/utils";

export function ntsStateDirectory(options: {
  projectRoot: string;
  override?: string;
  stateHome?: string;
}): string {
  if (options.override !== undefined) return resolve(expandPath(options.override));
  const home = resolve(
    expandPath(options.stateHome ?? process.env.XDG_STATE_HOME ?? join(homedir(), ".local/state")),
  );
  return join(home, "signalbox", "nts", hash(resolve(expandPath(options.projectRoot))).slice(0, 12));
}
