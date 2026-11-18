// Runs the nts host: the kit's serveProject with nts's config, once its checkout is there.
import { fileURLToPath } from "node:url";
import { serveProject } from "@signalbox/kit/host/serve";
import { createNtsProject, requireCheckout } from "./host/config.ts";
import { ntsConfig } from "./config.ts";

await requireCheckout(ntsConfig);
await serveProject({
  directory: fileURLToPath(new URL("./", import.meta.url)),
  title: "NTS",
  root: ntsConfig.root,
  state: ntsConfig.state,
  webPort: ntsConfig.webPort,
  apiPort: ntsConfig.apiPort,
  ...(ntsConfig.apiUrl ? { apiUrl: ntsConfig.apiUrl } : {}),
  // A tailnet that proxies to this host (tailscale serve) is read from Tailscale, not config.
  config: ({ web, tailnet }) =>
    createNtsProject(
      { ...ntsConfig, publicOrigins: [...ntsConfig.publicOrigins, ...tailnet] },
      web,
    ),
});
