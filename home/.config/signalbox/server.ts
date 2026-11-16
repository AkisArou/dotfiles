import { context as buildContext } from "esbuild";
import { mkdir, copyFile, cp } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { join } from "node:path";
import { startHost } from "signalbox/host";
import { createNtsProject, requireCheckout } from "./host/config.ts";
import { ntsConfig } from "./config.ts";

const project = fileURLToPath(new URL("./", import.meta.url));
const { webPort: port, apiPort, state: directory } = ntsConfig;
for (const value of [port, apiPort])
  if (!Number.isInteger(value) || value < 1 || value > 65535)
    throw new Error("NTS ports must be between 1 and 65535");
const publicOrigin = ntsConfig.origin ?? `http://127.0.0.1:${port}`;
const publicApi =
  ntsConfig.apiUrl ??
  (ntsConfig.origin
    ? `${new URL(publicOrigin).protocol === "https:" ? "wss:" : "ws:"}//${new URL(publicOrigin).host}/rpc`
    : `ws://127.0.0.1:${apiPort}/rpc`);
await requireCheckout(ntsConfig);
const host = await startHost(createNtsProject(ntsConfig), { stateDirectory: directory });
let assets: Awaited<ReturnType<typeof buildContext>> | undefined;
let stopping: Promise<void> | undefined;
const stop = () =>
  (stopping ??= (async () => {
    try {
      await assets?.dispose();
    } finally {
      await host.stop();
    }
  })());
try {
  const output = join(project, ".cache/web");
  await mkdir(output, { recursive: true });
  await copyFile(join(project, "web/index.html"), join(output, "index.html"));
  await copyFile(join(project, "web/manifest.webmanifest"), join(output, "manifest.webmanifest"));
  await copyFile(join(project, "web/sw.js"), join(output, "sw.js"));
  await cp(join(project, "web/icons"), join(output, "icons"), { recursive: true });
  assets = await buildContext({
    absWorkingDir: project,
    entryPoints: ["web/main.tsx"],
    outfile: join(output, "app.js"),
    bundle: true,
    format: "esm",
    platform: "browser",
    target: "es2022",
    jsx: "automatic",
    jsxImportSource: "react",
    tsconfig: "tsconfig.json",
    define: { SIGNALBOX_API_URL: JSON.stringify(publicApi) },
    sourcemap: true,
  });
  await assets.watch();
  await assets.serve({ host: "127.0.0.1", port, servedir: output });
  console.log(
    `\nSignalbox · NTS\n\n  Browser  http://127.0.0.1:${port}\n  TUI      pnpm --dir playground tui\n  API      ws://127.0.0.1:${apiPort}/rpc\n  Project  ${ntsConfig.root}\n  State    ${directory}\n\nGitHub issues and NTS status readers. Native terminals launch only on request.\n`,
  );
  process.once("SIGINT", () => {
    void stop().catch((error) => {
      console.error(error);
      process.exitCode = 1;
    });
  });
  process.once("SIGTERM", () => {
    void stop().catch((error) => {
      console.error(error);
      process.exitCode = 1;
    });
  });
} catch (error) {
  await stop();
  throw error;
}
