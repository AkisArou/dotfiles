// The Signalbox console on this machine: every project's pages and notifications,
// from the projects' directory (this folder's parent). Its ports are console.json's.
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serveConsole } from "@signalbox/kit/console/server";

const here = fileURLToPath(new URL("./", import.meta.url));
await serveConsole({
  registry: dirname(resolve(here)),
  directory: here,
  state: join(
    process.env.XDG_STATE_HOME ?? join(homedir(), ".local/state"),
    "signalbox/console",
  ),
});
