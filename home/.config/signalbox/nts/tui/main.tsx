/** @jsxImportSource @opentui/react */
import { createCliRenderer } from "@opentui/core";
import { createRoot } from "@opentui/react";
import { createClient, webSocketConnection } from "signalbox/client";
import { SignalboxProvider } from "signalbox/client/react";
import { TerminalApp } from "./app.tsx";
import { colors } from "./theme.ts";
import { terminalConfig } from "./config.ts";
import { createTerminalTools } from "./tools.ts";
import { TerminalTheme } from "./selection.tsx";
import { TerminalProvider } from "../shared/terminal.ts";
import { terminalUrl } from "signalbox/client/terminal";

// scripts/tui.ts passes this project's host (project.json), or sbn --api's.
const api =
  process.env.SIGNALBOX_API_URL ?? process.env.SIGNALBOX_NTS_API_URL ?? "ws://127.0.0.1:4311/rpc";
const client = createClient({ connect: webSocketConnection(api) });
let finish!: () => void;
const ended = new Promise<void>((resolve) => {
  finish = resolve;
});
const renderer = await createCliRenderer({
  screenMode: "alternate-screen",
  // The TUI draws no images. Detecting them sends a Kitty graphics query (an
  // APC string), which tmux takes as the pane's title.
  environment: { OPENTUI_GRAPHICS: "false" },
  backgroundColor: colors.background,
  exitOnCtrlC: false,
  onDestroy() {
    client.close();
    finish();
  },
});
// Name the terminal (and its tmux window) sbn@<project>; the launcher names the project.
const title = process.env.SIGNALBOX_PROJECT ? `sbn@${process.env.SIGNALBOX_PROJECT}` : undefined;
const nameTerminal = () => title && renderer.setTerminalTitle(title);
nameTerminal();
// A tool (fzf, lazygit, the editor) borrows the terminal through suspend, which
// clears the title: the window keeps its name while the tool runs, and after.
const tools = createTerminalTools(
  {
    suspend: async () => {
      await renderer.suspend();
      // Suspended, the renderer takes no title; the terminal is the tool's now, named directly (OSC 2).
      if (title && process.stdout.isTTY) process.stdout.write(`\x1b]2;${title}\x07`);
    },
    resume: async () => {
      await renderer.resume();
      nameTerminal();
    },
    get isDestroyed() {
      return renderer.isDestroyed;
    },
  },
  terminalConfig.finder,
);
createRoot(renderer).render(
  <SignalboxProvider client={client}>
    <TerminalTheme>
      <TerminalProvider url={terminalUrl(api)}>
        <TerminalApp tools={tools} onExit={() => renderer.destroy()} />
      </TerminalProvider>
    </TerminalTheme>
  </SignalboxProvider>,
);
client.start();
const stop = () => renderer.destroy();
process.once("SIGTERM", stop);
process.once("SIGINT", stop);
await ended;
await tools.close();
await renderer.closed;
process.off("SIGTERM", stop);
process.off("SIGINT", stop);
