import { join, resolve } from "node:path";
import { capability, Cell, provide, valueOf } from "signalbox/core";
import type { Capability, Extension } from "signalbox/core";
import { hash, poll, toJson, expandPath, Conflict } from "signalbox/core/utils";
import type { WorkflowStore } from "signalbox/ext/assignments/index";
import { tmux } from "signalbox/ext/integrations/tmux";
import { exposeTerminals } from "signalbox/ext/terminals/web";
import type { TerminalTarget } from "signalbox/ext/terminals/index";
import type { Agent } from "../shared/model.ts";
import type { NtsConfig } from "../config.ts";
import { discoverTerminals } from "./discovery.ts";

type Record = {
  id: string | null;
  label: string;
  program: string;
  path: string;
  requestId: string;
  createdAt: number;
};
interface NativeTerminals {
  agents: Cell<Agent[]>;
  start(program: string, requestId: string): Promise<{ id: string }>;
  resolve(id: string): Promise<TerminalTarget>;
}
export function nativeTerminals(options: {
  state: Capability<WorkflowStore>;
  directory: string;
  config: NtsConfig;
}) {
  const { terminalPrograms } = options.config;
  let attachedTerminals = [...options.config.attachedTerminals];
  let signal: AbortSignal;
  const root = resolve(expandPath(options.config.root));
  const execution = tmux({
    id: "native-terminals",
    socketName: `signalbox-${hash(options.directory).slice(0, 16)}`,
    state: options.state,
    directory: join(options.directory, "native-terminals"),
  });
  const service = capability<NativeTerminals>("nts/native-terminals");
  const agents = new Cell<Agent[]>([]);
  let state: WorkflowStore;
  let backend: import("signalbox/ext/execution/index").ExecutionBackend;
  const records = () =>
    state.transaction("nts/native-terminals", (tx) =>
      tx.entries("").map((entry) => entry.value as unknown as Record),
    );
  const refresh = async () => {
    attachedTerminals = [
      ...options.config.attachedTerminals,
      ...(await discoverTerminals(options.config, signal)),
    ];
    const all = await records(),
      executions = valueOf(backend.executions) ?? [];
    // Recover a launch whose acknowledgement/catalog update was interrupted.
    for (const record of all)
      if (!record.id) {
        const recovered = await backend.recover(record.requestId);
        if (recovered) {
          record.id = recovered.id;
          await state.transaction("nts/native-terminals", (tx) =>
            tx.set(record.requestId, toJson(record)),
          );
        }
      }
    agents.set([
      ...attachedTerminals.map((terminal) => ({
        id: `attached/${terminal.id}`,
        name: terminal.label,
        role: "native",
        path: terminal.path,
        state: "external",
        issue: null,
        terminal: `attached/${terminal.id}`,
      })),
      ...all.flatMap((record) =>
        record.id
          ? [
              {
                id: record.id,
                name: record.label,
                role: record.program,
                path: record.path,
                state:
                  executions.find((execution) => execution.id === record.id)?.state ?? "unknown",
                issue: null,
                terminal: record.id,
              },
            ]
          : [],
      ),
    ]);
  };
  const instance: NativeTerminals = {
    agents,
    async start(id, requestId) {
      const program = terminalPrograms.find((program) => program.id === id);
      if (!program) throw new Error("Unknown terminal program");
      await state.transaction("nts/native-terminals", (tx) => {
        const previous = tx.get(requestId) as unknown as Record | undefined;
        if (previous && (previous.program !== id || previous.path !== root))
          throw new Conflict("Terminal request changed");
        if (!previous)
          tx.set(
            requestId,
            toJson({
              id: null,
              label: program.label,
              program: id,
              requestId,
              path: root,
              createdAt: Date.now(),
            }),
          );
      });
      const execution = await backend.launch({ ...program, requestId, cwd: root, terminal: true });
      await state.transaction("nts/native-terminals", (tx) => {
        const record = tx.get(requestId) as unknown as Record;
        tx.set(requestId, toJson({ ...record, id: execution.id }));
      });
      await refresh();
      return { id: execution.id };
    },
    async resolve(id) {
      const attached = attachedTerminals.find((terminal) => `attached/${terminal.id}` === id);
      if (attached) return attached.target;
      if (!(await records()).some((record) => record.id === id))
        throw new Error("Unknown native terminal");
      return backend.terminal!(id);
    },
  };
  const extension: Extension = {
    id: "nts-native-terminals",
    requires: [options.state, execution.backend],
    provides: [provide(service, instance)],
    async start(context) {
      signal = context.signal;
      state = context.get(options.state);
      backend = context.get(execution.backend);
      await refresh().catch((error) => agents.fail(error));
      context.afterStart(() => {
        const loop = poll(refresh, 1000, context.signal, (error) => agents.fail(error));
        context.onStop(() => loop);
      });
    },
  };
  return {
    service,
    extensions: [
      execution.extension,
      extension,
      exposeTerminals({
        requires: [service],
        resolve: (id, context) => context.get(service).resolve(id),
      }),
    ],
  };
}
