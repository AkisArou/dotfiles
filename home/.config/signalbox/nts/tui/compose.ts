// Words you write in your editor: a written answer, a note for a worker, the
// reason for a cancellation. Lines starting with # explain and are left out.
import { mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import type { TerminalCommand, TerminalTools } from "./tools.ts";

/** Writes text in the editor; the notes are shown as # lines. An empty text is undefined. */
export async function written(
  tools: TerminalTools,
  editor: TerminalCommand,
  name: string,
  notes: readonly string[],
): Promise<string | undefined> {
  const directory = join(process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache"), "signalbox-tui");
  await mkdir(directory, { recursive: true });
  const file = join(directory, `answer-${name.replace(/[^A-Za-z0-9_.-]/g, "_")}.md`);
  await writeFile(
    file,
    [
      "",
      "# Write above this line. Lines starting with # are left out; leaving it empty cancels.",
      ...notes.map((note) => `# ${note}`),
      "",
    ].join("\n"),
  );
  try {
    await tools.run({ ...editor, args: [...(editor.args ?? []), file] }, directory);
    const text = (await readFile(file, "utf8"))
      .split("\n")
      .filter((line) => !line.startsWith("#"))
      .join("\n")
      .trim();
    return text || undefined;
  } finally {
    await rm(file, { force: true });
  }
}
