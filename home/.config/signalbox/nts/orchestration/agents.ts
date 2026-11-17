// The managed agent runtime: Claude Code and Codex sessions in a tmux server of
// their own, each with the Signalbox tools its role is granted. A profile picks
// the program and its launch options; the runtime starts and recovers sessions.
import { join } from "node:path";
import { z } from "zod";
import type { Capability } from "signalbox/core";
import type { Json } from "signalbox/core/remote";
import { hash } from "signalbox/core/utils";
import { agentProfile, agentRuntime, agentToolBridge, textInput } from "signalbox/ext/agents";
import type {
  AgentInstance,
  AgentProgram,
  AgentRuntime,
  AgentToolset,
  ToolInput,
} from "signalbox/ext/agents";
import type { WorkflowStore } from "signalbox/ext/assignments";
import { claudeCode } from "signalbox/ext/integrations/claude";
import { codex } from "signalbox/ext/integrations/codex";
import { tmux } from "signalbox/ext/integrations/tmux";
import type { Harness, Profile } from "./models.ts";
import { launchOptions } from "./models.ts";
import { agentTemp } from "./agent-temp.ts";

/** A zod schema as a tool's input: its parser and the JSON Schema the harness shows the model. */
export function toolInput<T extends z.ZodType<Json>>(schema: T): ToolInput<z.infer<T>> {
  return {
    parser: { parse: (value: unknown) => schema.parse(value) as z.infer<T> },
    parameters: z.toJSONSchema(schema) as Json,
  };
}

export function managedAgents(options: {
  readonly state: Capability<WorkflowStore>;
  readonly directory: string;
  readonly toolsets: readonly AgentToolset[];
}) {
  const execution = tmux({
    id: "nts-agents-tmux",
    // tmux socket names are short; derive one from the state directory so two
    // hosts on different states never share a server.
    socketName: `sb-agents-${hash(options.directory).slice(0, 12)}`,
    state: options.state,
    directory: join(options.directory, "agents/execution"),
  });
  const bridge = agentToolBridge({
    id: "nts-tools",
    state: options.state,
    directory: join(options.directory, "agents/tools"),
    toolsets: options.toolsets,
  });
  const temp = (() => {
    const directory = join(options.directory, "agents/claude-tmp");
    return { directory, extension: agentTemp({ directory }) };
  })();
  const common = {
    state: options.state,
    execution: execution.backend,
    bridge: bridge.bridge,
    allowMountedTools: true,
  };
  const programs: Record<Harness, AgentProgram> = {
    claude: claudeCode({
      ...common,
      id: "claude",
      directory: join(options.directory, "agents/claude"),
      // These sessions bypass permission prompts (the user's settings), so Claude
      // Code would hold every Signalbox message for approval. Accepting inbound
      // messages applies to Signalbox's own sessions only, not the user's others;
      // it does trust every local session of this user to message them.
      settings: { crossSessionInbound: "accept" },
      // Temporary files on disk, aged by the host (agentTemp), not in /tmp's small tmpfs quota.
      env: { CLAUDE_CODE_TMPDIR: temp.directory },
    }),
    codex: codex({ ...common, id: "codex", directory: join(options.directory, "agents/codex") }),
  };
  const runtime = agentRuntime({ id: "nts-agents", programs: Object.values(programs) });
  return {
    runtime: runtime.runtime,
    instances: runtime.instances,
    extensions: [execution.extension, bridge.extension, temp.extension, runtime.extension],
    /** Start one session for a resolved profile. requestId is the durable launch identity. */
    start(
      agents: AgentRuntime,
      request: {
        readonly profile: Profile;
        readonly label: string;
        readonly cwd: string;
        readonly brief: string;
        readonly instructions: readonly string[];
        readonly tools: readonly AgentToolset[];
        readonly requestId: string;
      },
      signal: AbortSignal,
    ): Promise<AgentInstance> {
      return agents.start(
        {
          // Role instructions become the session's system prompt; the brief is its first message.
          profile: agentProfile({
            program: programs[request.profile.harness],
            instructions: request.instructions,
          }),
          label: request.label,
          cwd: request.cwd,
          input: textInput(request.brief),
          tools: request.tools,
          options: launchOptions(request.profile),
          requestId: request.requestId,
        },
        signal,
      );
    },
  };
}
export type ManagedAgents = ReturnType<typeof managedAgents>;
