/** @jsxImportSource @opentui/react */
import { runTerminalApp } from "@signalbox/kit/tui/run";
import { app } from "./project.tsx";

// scripts/tui.ts (the kit's launcher) passes this project's host, or sbn --api's.
const api = process.env.SIGNALBOX_API_URL;
if (!api) throw new Error("SIGNALBOX_API_URL names the host; start the TUI with sbn");
await runTerminalApp(app, { api });
