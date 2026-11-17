import { build } from "esbuild";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";
import { readdir, readFile } from "node:fs/promises";

const [major, minor] = process.versions.node.split(".").map(Number);
if (major! < 26 || (major === 26 && minor! < 4))
  throw new Error("The OpenTUI playground needs Node 26.4 or later");
const root = fileURLToPath(new URL("../", import.meta.url));
const testing = process.argv.includes("--test");
const entries = testing
  ? (await readdir(resolve(root, "tui")))
      .filter((file) => /\.test\.tsx?$/.test(file))
      .sort()
      .map((file) => `tui/${file}`)
  : ["tui/main.tsx"];
const output = resolve(root, ".cache/tui");
await build({
  absWorkingDir: root,
  entryPoints: entries,
  outdir: output,
  outExtension: { ".js": ".mjs" },
  bundle: true,
  packages: "external",
  platform: "node",
  target: "node26",
  format: "esm",
  jsx: "automatic",
  jsxImportSource: "@opentui/react",
  tsconfig: "tsconfig.tui.json",
  sourcemap: true,
});
// The TUI talks to this project's host unless SIGNALBOX_API_URL says otherwise.
const project = JSON.parse(await readFile(resolve(root, "project.json"), "utf8")) as {
  apiPort: number;
};
const env = {
  ...process.env,
  SIGNALBOX_API_URL:
    process.env.SIGNALBOX_API_URL ??
    process.env.SIGNALBOX_NTS_API_URL ??
    `ws://127.0.0.1:${project.apiPort}/rpc`,
};
const child = spawn(
  process.execPath,
  [
    "--experimental-ffi",
    ...(testing ? ["--test"] : []),
    ...entries.map((entry) => resolve(output, entry.slice(4).replace(/\.tsx?$/, ".mjs"))),
  ],
  { stdio: "inherit", env },
);
child.on("error", (error) => {
  console.error(error);
  process.exitCode = 1;
});
child.on("exit", (code) => {
  process.exitCode = code ?? 1;
});
