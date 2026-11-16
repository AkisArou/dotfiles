import { readFile, stat } from "node:fs/promises";
import { join } from "node:path";
import { IncomingMessage } from "node:http";
import { z } from "zod";
import { Cell, defineConfig } from "signalbox/core";
import type { Extension, Readable } from "signalbox/core";
import type { Json, SourceRef } from "signalbox/core/remote";
import { poll } from "signalbox/core/utils";
import { exposeSource, exposeOperation, webListener } from "signalbox/host";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import * as refs from "../shared/model.ts";
import type { NtsConfig } from "../config.ts";
import { ntsObservations } from "../nts/host.ts";
import { nativeTerminals } from "./terminals.ts";
import { projectIssues, readIssues } from "./tasks.ts";

// Live NTS observations and native terminal hosting. NTS tools retain ownership
// of workspaces and landing; coordinator sessions stay native.
export function createNtsProject(settings: NtsConfig) {
  const state = sqliteWorkflowState({ path: join(settings.state, "runtime.sqlite") });
  const native = nativeTerminals({
    state: state.state,
    directory: settings.state,
    config: settings,
  });
  const startedAt = Date.now();
  function expose<T extends Json>(ref: SourceRef<T>, cell: Readable<T>) {
    return exposeSource({ ref, requires: [], output: z.custom<T>(), resolve: () => cell });
  }
  function observe<T extends Json>(
    ref: SourceRef<T>,
    interval: number,
    read: (signal: AbortSignal) => Promise<T>,
  ): Extension {
    const cell = new Cell<T>();
    return {
      id: `${ref.id}/reader`,
      extensions: [expose(ref, cell)],
      start(context) {
        context.afterStart(() => {
          const loop = poll(
            async () => cell.set(await read(context.signal)),
            interval,
            context.signal,
            (error) => cell.fail(error),
          );
          context.onStop(() => loop);
        });
      },
    };
  }
  const planPath = join(settings.root, settings.plan);
  const origin = settings.origin ?? `http://127.0.0.1:${settings.webPort}`;
  return defineConfig({
    id: "nts",
    title: "NTS",
    root: settings.root,
    gate: {
      accept(connection) {
        const request = connection.request;
        if (!(request instanceof IncomingMessage)) return false;
        if (!["127.0.0.1", "::1", "::ffff:127.0.0.1"].includes(request.socket.remoteAddress ?? ""))
          return false;
        if (request.headers.origin && request.headers.origin !== origin) return false;
        return { context: {} };
      },
    },
    extensions: [
      state.extension,
      ...native.extensions,
      expose(
        refs.overview,
        new Cell({ name: "nts", root: settings.root, repo: settings.repo, startedAt }),
      ),
      observe(refs.plan, 30_000, () => readFile(planPath, "utf8")),
      observe(refs.issues, settings.issueIntervalMs, async (signal) => {
        const listed = await readIssues(settings.repo, settings.issueLimit, signal);
        // The plan has its own failure reporting; issue visibility doesn't depend on it.
        const plan = await readFile(planPath, "utf8").catch((error: NodeJS.ErrnoException) => {
          if (error.code === "ENOENT") return "";
          throw error;
        });
        return projectIssues(listed, plan);
      }),
      exposeSource({
        ref: refs.agents,
        requires: [native.service],
        output: z.custom<refs.Agent[]>(),
        resolve: (context) => context.get(native.service).agents,
      }),
      expose(
        refs.terminalPrograms,
        new Cell(settings.terminalPrograms.map(({ id, label }) => ({ id, label }))),
      ),
      exposeOperation({
        ref: refs.startTerminal,
        requires: [native.service],
        input: z.object({ program: z.string().min(1) }),
        output: z.object({ id: z.string() }),
        run: (input, context) =>
          context.get(native.service).start(input.program, context.requestId),
      }),
      ntsObservations({ root: settings.root }),
      webListener({ hostname: "127.0.0.1", port: settings.apiPort }),
    ],
  });
}

export async function requireCheckout(settings: NtsConfig) {
  if (!(await stat(settings.root).catch(() => undefined))?.isDirectory())
    throw new Error(
      `NTS checkout not found: ${settings.root}. Set NTS_ROOT to your existing checkout; run pnpm --dir playground run doctor for prerequisites.`,
    );
  if (
    !(await stat(join(settings.root, "tooling/landing/check.ts")).catch(() => undefined))?.isFile()
  )
    throw new Error(`Not an NTS checkout: ${settings.root} (missing tooling/landing/check.ts)`);
}
