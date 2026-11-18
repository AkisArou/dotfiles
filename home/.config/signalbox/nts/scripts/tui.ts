// nts's terminal app (or, with --test, its terminal tests), launched by the kit.
import { fileURLToPath } from "node:url";
import { launchTerminalApp } from "@signalbox/kit/tui/launch";

process.exitCode = await launchTerminalApp({
  directory: fileURLToPath(new URL("../", import.meta.url)),
  testing: process.argv.includes("--test"),
});
