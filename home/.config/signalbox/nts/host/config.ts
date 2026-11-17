import { readFile, stat } from "node:fs/promises";
import { join } from "node:path";
import { IncomingMessage } from "node:http";
import { z } from "zod";
import { Cell, combineReadables, defineConfig, polledSource } from "signalbox/core";
import type { Readable } from "signalbox/core";
import type { Json, SourceRef } from "signalbox/core/remote";
import { exposeOperation, exposeSource, staticListener, webListener } from "signalbox/host";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import * as refs from "../shared/model.ts";
import type { NtsConfig } from "../config.ts";
import { ntsObservations } from "../status/host.ts";
import { nativeTerminals } from "./terminals.ts";
import { projectIssues, readIssues } from "./tasks.ts";
import { ntsOrchestration } from "../orchestration/index.ts";
import { gitHub } from "../orchestration/needs.ts";
import { orchestrationEndpoints, managedAgentViews } from "../orchestration/endpoints.ts";
import { nativeAttachment } from "signalbox/ext/agents";
import type { TerminalTarget } from "signalbox/ext/terminals";

// Live NTS observations and native terminal hosting. NTS tools retain ownership
// of workspaces and landing; coordinator sessions stay native.
/** The NTS host; `web` is the built browser UI it serves on the web port. */
export function createNtsProject(settings: NtsConfig, web: { readonly directory: string }) {
  const state = sqliteWorkflowState({ path: join(settings.state, "runtime.sqlite") });
  let resolveManaged: ((id: string) => Promise<TerminalTarget | undefined>) | undefined;
  const native = nativeTerminals({
    state: state.state,
    directory: settings.state,
    config: settings,
    fallback: (id) => resolveManaged?.(id) ?? Promise.resolve(undefined),
  });
  const startedAt = Date.now();
  function expose<T extends Json>(ref: SourceRef<T>, cell: Readable<T>) {
    return exposeSource({ ref, requires: [], output: z.custom<T>(), resolve: () => cell });
  }
  /** A polled reading exposed under its ref; demand as in polledSource. */
  function observe<T extends Json>(
    ref: SourceRef<T>,
    interval: number,
    read: (signal: AbortSignal) => Promise<T>,
    demand: "always" | "subscribed" = "always",
  ) {
    const source = polledSource<T>({ id: `${ref.id}/reading`, interval, read, demand });
    return {
      key: source.key,
      refresh: source.refresh,
      extensions: [source.extension, expose(ref, source.readable)],
    };
  }
  const planPath = join(settings.root, settings.plan);
  const issues = observe(refs.issues, settings.issueIntervalMs, async (signal) => {
    const listed = await readIssues(settings.repo, settings.issueLimit, signal);
    // The plan has its own failure reporting; issue visibility doesn't depend on it.
    const plan = await readFile(planPath, "utf8").catch((error: NodeJS.ErrnoException) => {
      if (error.code === "ENOENT") return "";
      throw error;
    });
    return projectIssues(listed, plan);
  });
  const orchestration = ntsOrchestration({
    root: settings.root,
    directory: settings.state,
    state: state.state,
    issues: issues.key,
    settings: settings.orchestration,
    gitHub: gitHub(settings.repo),
    refreshIssues: issues.refresh,
  });
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
      // The plan page is only viewed; issues feed admission and the coordinator.
      ...observe(refs.plan, 30_000, () => readFile(planPath, "utf8"), "subscribed").extensions,
      ...issues.extensions,
      ...orchestration.extensions,
      ...orchestrationEndpoints(orchestration),
      {
        id: "nts-managed-terminals",
        requires: [orchestration.agents.runtime],
        start(context) {
          const runtime = context.get(orchestration.agents.runtime);
          resolveManaged = async (id) => runtime.capability(id, nativeAttachment)?.terminal?.();
        },
      },
      exposeSource({
        ref: refs.agents,
        requires: [native.service, orchestration.agents.runtime],
        output: z.custom<refs.Agent[]>(),
        resolve: (context) =>
          combineReadables(
            [
              context.get(native.service).agents,
              context.get(orchestration.agents.runtime).instances,
            ] as const,
            (terminals, managed) => [...managedAgentViews(managed), ...terminals],
          ),
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
      ntsObservations({
        root: settings.root,
        landing: orchestration.landing,
        fullCheck: orchestration.fullCheck,
      }),
      webListener({ hostname: "127.0.0.1", port: settings.apiPort }),
      staticListener({ hostname: "127.0.0.1", port: settings.webPort, directory: web.directory }),
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
