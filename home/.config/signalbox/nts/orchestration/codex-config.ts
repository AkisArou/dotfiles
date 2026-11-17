// The model Codex runs when a profile names none ("configured"): the top-level
// `model` in its config.toml, so lists can name it rather than say "configured".
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

/** The top-level model a Codex config.toml sets, before any [section]. */
export function codexModelIn(toml: string): string | undefined {
  for (const line of toml.split("\n")) {
    if (/^\s*\[/.test(line)) return undefined;
    const match = /^\s*model\s*=\s*"([^"]+)"/.exec(line);
    if (match) return match[1];
  }
  return undefined;
}

let read: { model: string | undefined } | undefined;
/** Codex's configured model on this machine, read once. */
export function configuredCodexModel(): string | undefined {
  read ??= (() => {
    try {
      const home = process.env.CODEX_HOME ?? join(homedir(), ".codex");
      return { model: codexModelIn(readFileSync(join(home, "config.toml"), "utf8")) };
    } catch {
      return { model: undefined };
    }
  })();
  return read.model;
}
