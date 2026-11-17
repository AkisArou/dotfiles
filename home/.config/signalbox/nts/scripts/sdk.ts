// Build the Signalbox SDK from its checkout, then refresh this application's
// injected copy of it. The SDK's own build script compiles it; pnpm re-links the
// copy only when the built files changed.
import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";

const root = fileURLToPath(new URL("../", import.meta.url));
const manifest = JSON.parse(readFileSync(resolve(root, "package.json"), "utf8")) as {
  dependencies: Record<string, string>;
};
const spec = manifest.dependencies.signalbox ?? "";
if (!spec.startsWith("file:")) throw new Error(`signalbox must be a file: dependency, not ${spec}`);
const sdk = resolve(root, spec.slice("file:".length));

function run(command: string, args: readonly string[], cwd: string): void {
  const result = spawnSync(command, args, { cwd, stdio: "inherit" });
  if (result.error) throw result.error;
  if (result.status !== 0) throw new Error(`${command} ${args.join(" ")} exited ${result.status}`);
}

// npm_execpath is the pnpm running this script (a native executable or a JS entry).
const pnpm = process.env.npm_execpath;
if (!pnpm) throw new Error("Run through pnpm (pnpm sdk), so the injected SDK copy can be refreshed");
const javascript = /\.[cm]?js$/.test(pnpm);
const viaPnpm = (args: readonly string[], cwd: string) =>
  run(javascript ? process.execPath : pnpm, [...(javascript ? [pnpm] : []), ...args], cwd);

// The SDK's own build (a clean compile), then the workspace root (one level up)
// refreshes every project's injected copy offline.
viaPnpm(["run", "build"], sdk);
viaPnpm(["install", "--offline", "--silent"], resolve(root, ".."));
