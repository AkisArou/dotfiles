// Runs the nts host: the kit's serveProject with nts's config, once its checkout is there.
import { serveProject } from "@signalbox/kit/host/serve";
import { createNtsProject, requireCheckout } from "./host/config.ts";
import { ntsConfig } from "./config.ts";

await requireCheckout(ntsConfig);
await serveProject({
  title: ntsConfig.title,
  root: ntsConfig.root,
  state: ntsConfig.state,
  apiPort: ntsConfig.apiPort,
  config: (served) => createNtsProject(ntsConfig, served),
});
