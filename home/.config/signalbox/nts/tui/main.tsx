/** @jsxImportSource @opentui/react */
import { runTerminalApp } from "@signalbox/kit/tui/run";
import { app } from "./project.tsx";

// scripts/tui.ts passes this project's host (project.json), or sbn --api's.
await runTerminalApp(app, {
  api:
    process.env.SIGNALBOX_API_URL ?? process.env.SIGNALBOX_NTS_API_URL ?? "ws://127.0.0.1:4311/rpc",
});
