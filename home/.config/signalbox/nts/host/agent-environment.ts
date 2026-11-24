// The environment nts's agents run in: the host's, without what asdf's exec of the
// host's own node put there. The host starts through asdf's node shim, whose exec puts
// that node's install (and its plugin's shims) ahead of ~/.asdf/shims and sets
// ASDF_INSTALL_*. Inherited, those would pin every agent to the host's node; nts pins
// its node per tree (.tool-versions), which ~/.asdf/shims resolves by directory.
import { homedir } from "node:os";
import { join } from "node:path";
import type { AgentEnvironment } from "@signalbox/kit/orchestration/agents";

/** The variables asdf's exec sets for the program it runs. */
const EXEC_VARIABLES = ["ASDF_INSTALL_TYPE", "ASDF_INSTALL_VERSION", "ASDF_INSTALL_PATH"];

export function agentEnvironment(env: NodeJS.ProcessEnv = process.env): AgentEnvironment {
  const data = env.ASDF_DATA_DIR ?? join(homedir(), ".asdf");
  const installs = join(data, "installs") + "/";
  const plugins = join(data, "plugins") + "/";
  // An install's own directories, and a plugin's shims, are on PATH only through exec:
  // asdf's own setup puts only its shims directory there.
  const injected = (entry: string) =>
    entry.startsWith(installs) || (entry.startsWith(plugins) && entry.endsWith("/shims"));
  return {
    ...(env.PATH === undefined
      ? {}
      : {
          env: {
            PATH: env.PATH.split(":")
              .filter((entry) => !injected(entry))
              .join(":"),
          },
        }),
    unsetEnv: EXEC_VARIABLES,
  };
}
