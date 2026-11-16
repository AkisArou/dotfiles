// Local viewer tools. They borrow the terminal; they do not run on the host.
import { spawn } from "node:child_process";
import { stat } from "node:fs/promises";
import type { ChildProcess } from "node:child_process";
import type { CliRenderer } from "@opentui/core";

export interface TerminalCommand {
  readonly command: string;
  readonly args?: readonly string[];
}
export interface TerminalTools {
  find(lines: readonly string[], title: string): Promise<number | undefined>;
  run(command: TerminalCommand, cwd: string): Promise<void>;
}
type Terminal = Pick<CliRenderer, "suspend" | "resume" | "isDestroyed">;

export function createTerminalTools(
  terminal: Terminal,
  finder: (title: string) => TerminalCommand,
) {
  let active: ChildProcess | undefined;
  let running: Promise<unknown> | undefined;
  let closed = false;
  let closing: Promise<void> | undefined;

  async function borrow<T>(action: () => Promise<T>): Promise<T> {
    if (closed) throw new Error("Terminal tools are closed");
    if (running) throw new Error("Another tool is using the terminal");
    const operation = (async () => {
      await terminal.suspend();
      try {
        if (closed) throw new Error("Terminal tools are closed");
        return await action();
      } finally {
        if (!terminal.isDestroyed) await terminal.resume();
      }
    })();
    running = operation;
    try {
      return await operation;
    } finally {
      running = undefined;
    }
  }

  function execute(
    spec: TerminalCommand,
    options: { cwd?: string; input?: string; capture?: boolean },
  ) {
    return new Promise<{ code: number | null; signal: NodeJS.Signals | null; output: string }>(
      (resolve, reject) => {
        const child = spawn(spec.command, [...(spec.args ?? [])], {
          cwd: options.cwd,
          env: process.env,
          shell: false,
          stdio: options.capture ? ["pipe", "pipe", "inherit"] : "inherit",
        });
        active = child;
        const chunks: Buffer[] = [];
        let bytes = 0,
          failure: Error | undefined;
        child.on("error", (error: NodeJS.ErrnoException) => {
          failure =
            error.code === "ENOENT"
              ? new Error(
                  `${spec.command} is not installed; edit tui/config.ts to choose another tool`,
                )
              : error;
        });
        child.stdout?.on("data", (chunk: Buffer) => {
          bytes += chunk.length;
          if (bytes > 1024 * 1024) {
            failure = new Error("Finder output exceeded 1 MiB");
            child.kill();
          } else chunks.push(chunk);
        });
        child.stdin?.on("error", (error: NodeJS.ErrnoException) => {
          // A finder may close its input before all candidates are written.
          if (error.code !== "EPIPE") failure = error;
        });
        child.once("close", (code, signal) => {
          active = undefined;
          if (failure) reject(failure);
          else resolve({ code, signal, output: Buffer.concat(chunks).toString("utf8") });
        });
        child.stdin?.end(options.input);
      },
    );
  }

  return {
    async find(lines: readonly string[], title: string): Promise<number | undefined> {
      if (!lines.length) return undefined;
      // A candidate is one terminal-safe line; never shell code or an ANSI escape.
      const candidates = lines.map((line) => line.replace(/[\x00-\x1f\x7f-\x9f]/g, " "));
      return borrow(async () => {
        const command = finder(title);
        const result = await execute(command, {
          capture: true,
          input: `${candidates.join("\n")}\n`,
        });
        if (result.code === 1 || result.code === 130 || result.signal === "SIGINT")
          return undefined;
        if (result.code !== 0)
          throw new Error(`${command.command} exited with ${result.signal ?? result.code}`);
        const selected = result.output.replace(/\r?\n$/, "");
        const index = candidates.indexOf(selected);
        if (index < 0) throw new Error("Finder must print exactly one of the candidate lines");
        return index;
      });
    },
    async run(command: TerminalCommand, cwd: string): Promise<void> {
      const directory = await stat(cwd).catch(() => undefined);
      if (!directory?.isDirectory())
        throw new Error(
          `Workspace is unavailable on this machine: ${cwd}. G opens the main repository.`,
        );
      await borrow(async () => {
        const result = await execute(command, { cwd });
        if (result.code !== 0 && result.code !== 130 && result.signal !== "SIGINT")
          throw new Error(`${command.command} exited with ${result.signal ?? result.code}`);
      });
    },
    close(): Promise<void> {
      closed = true;
      return (closing ??= (async () => {
        const child = active;
        child?.kill("SIGTERM");
        const force = child ? setTimeout(() => child.kill("SIGKILL"), 1500) : undefined;
        force?.unref();
        try {
          await running?.catch(() => {});
        } finally {
          clearTimeout(force);
        }
      })());
    },
  } satisfies TerminalTools & { close(): Promise<void> };
}
