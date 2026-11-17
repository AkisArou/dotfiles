// Agent sessions' temporary files live on disk, in the host's state, not in
// /tmp: /tmp is a small tmpfs with a per-user quota, and one session's large
// scratch file there once stopped every session's shell. Nothing clears a
// folder on disk at reboot, so the host ages it as tmpfiles.d would: a file
// untouched for maxAgeMs goes, then each folder that leaves empty and itself
// untouched for as long.
import { lstat, mkdir, readdir, rm, rmdir } from "node:fs/promises";
import { join } from "node:path";
import type { Extension } from "signalbox/core";
import { poll } from "signalbox/core/utils";

export const WEEK_MS = 7 * 24 * 3_600_000;

/** Remove what has aged under root (never root itself); symbolic links are removed, never followed. */
export async function removeAged(
  root: string,
  maxAgeMs: number,
  now = Date.now(),
): Promise<{ files: number; folders: number }> {
  const removed = { files: 0, folders: 0 };
  const visit = async (folder: string): Promise<boolean> => {
    // Its time before this pass: removing its entries touches it.
    const touched = (await lstat(folder).catch(() => undefined))?.mtimeMs ?? now;
    const entries = await readdir(folder, { withFileTypes: true }).catch(() => []);
    let left = entries.length;
    for (const entry of entries) {
      const path = join(folder, entry.name);
      if (entry.isDirectory()) {
        if (await visit(path)) left--;
        continue;
      }
      const stat = await lstat(path).catch(() => undefined);
      if (stat && now - stat.mtimeMs > maxAgeMs) {
        await rm(path, { force: true });
        removed.files++;
        left--;
      }
    }
    if (folder === root || left > 0 || now - touched <= maxAgeMs) return false;
    // A file written meanwhile keeps the folder.
    return rmdir(folder).then(
      () => (removed.folders++, true),
      () => false,
    );
  };
  await visit(root);
  return removed;
}

/** The folder sessions get as their temporary directory, aged hourly while the host runs. */
export function agentTemp(options: { directory: string; maxAgeMs?: number }): Extension {
  return {
    id: "nts-agent-temp",
    requires: [],
    async start(context) {
      await mkdir(options.directory, { recursive: true, mode: 0o700 });
      context.afterStart(() => {
        const loop = poll(
          async () => {
            const removed = await removeAged(options.directory, options.maxAgeMs ?? WEEK_MS);
            if (removed.files || removed.folders)
              context.log.info(
                `agent temp: removed ${removed.files} aged files and ${removed.folders} folders`,
              );
          },
          3_600_000,
          context.signal,
          (error) => context.log.error("agent temp", error),
        );
        context.onStop(() => loop);
      });
    },
  };
}
