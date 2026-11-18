// The Signalbox console on this machine: every project's pages and notifications,
// from the projects' directory (~/.config/signalbox). Its ports are console.json's.
import { homedir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { serveConsole } from "@signalbox/kit/console/server";

const here = fileURLToPath(new URL("./", import.meta.url));
await serveConsole({
  directory: here,
  state: join(
    process.env.XDG_STATE_HOME ?? join(homedir(), ".local/state"),
    "signalbox/console",
  ),
});
