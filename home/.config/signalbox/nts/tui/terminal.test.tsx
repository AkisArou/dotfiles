/** @jsxImportSource @opentui/react */
import { test } from "node:test";
import assert from "node:assert/strict";
import { act } from "react";
import { testRender } from "@opentui/react/test-utils";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdtemp, readFile, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { defineConfig } from "signalbox/core";
import { startHost, webListener } from "signalbox/host";
import { exposeTerminals } from "signalbox/ext/terminals/web";
import { tmuxTerminal } from "signalbox/ext/integrations/tmux/terminal";
import { createClient } from "signalbox/client";
import { SignalboxProvider } from "signalbox/client/react";
import { TerminalProvider } from "../shared/terminal.ts";
import { TerminalTheme } from "./selection.tsx";
import { TerminalApp } from "./app.tsx";

test("the embedded native terminal owns keys and paste; Ctrl+] returns to navigation without ending its program", async () => {
  const root = await mkdtemp(join(tmpdir(), "signalbox-native-ui-")),
    socket = `sb-ui-${process.pid}-${Date.now()}`;
  const run = promisify(execFile),
    input = join(root, "input"),
    script = join(root, "program.cjs");
  const tmux = (args: string[]) => run("tmux", ["-L", socket, ...args]);
  await writeFile(input, "");
  await writeFile(
    script,
    `const fs=require('node:fs');process.stdin.setRawMode(true);process.stdout.write('\\x1b[32mNATIVE_TUI_READY\\x1b[0m\\r\\n');process.stdin.on('data',data=>{fs.appendFileSync(${JSON.stringify(input)},data);process.stdout.write('Received '+JSON.stringify(data.toString())+'\\r\\n')});`,
  );
  let port = 0,
    exits = 0;
  const calls: string[] = [];
  const values: Record<string, unknown> = {
    "nts.overview": {
      name: "test",
      root,
      repo: "AkisArou/nts",
      startedAt: 1,
    },
    "nts.issues": [],
    "nts.plan": "# Plan",
    "nts.terminal-programs": [],
    "nts.agents": [
      {
        id: "native",
        name: "Native fixture",
        terminal: "native",
        role: "native",
        state: "running",
        path: root,
        issue: null,
      },
    ],
  };
  const client = createClient({
    reconnect: false,
    connect: async (handlers) => ({
      send(request) {
        if (request.type === "subscribe")
          queueMicrotask(() =>
            handlers.message({
              id: request.id,
              result: { status: "ready", value: values[request.ref!], observedAt: 1 },
              event: true,
            }),
          );
        if (request.type === "call") {
          calls.push(request.ref!);
          queueMicrotask(() => handlers.message({ id: request.id, result: { accepted: true } }));
        }
      },
      close() {},
    }),
  });
  let host: Awaited<ReturnType<typeof startHost>> | undefined;
  let setup: Awaited<ReturnType<typeof testRender>> | undefined;
  try {
    await run("tmux", [
      "-L",
      socket,
      "-f",
      "/dev/null",
      "new-session",
      "-d",
      "-s",
      "agent",
      process.execPath,
      script,
    ]);
    host = await startHost(
      defineConfig({
        id: "native-ui",
        root,
        gate: { accept: () => ({ context: {} }) },
        extensions: [
          exposeTerminals({
            requires: [],
            resolve: () => tmuxTerminal({ target: "agent", server: { socketName: socket } }),
          }),
          webListener({
            hostname: "127.0.0.1",
            port: 0,
            onListening: (address) => {
              port = address.port;
            },
          }),
        ],
      }),
      { stateDirectory: join(root, "state") },
    );
    setup = await testRender(
      <SignalboxProvider client={client}>
        <TerminalTheme>
          <TerminalProvider url={`ws://127.0.0.1:${port}/terminals`}>
            <TerminalApp
              tools={{
                find: async () => undefined,
                findMany: async () => undefined,
                run: async () => {},
              }}
              onExit={() => {
                exits++;
              }}
            />
          </TerminalProvider>
        </TerminalTheme>
      </SignalboxProvider>,
      { width: 90, height: 32, kittyKeyboard: true, exitOnCtrlC: false },
    );
    const screen = setup;
    const wait = async (check: () => boolean | Promise<boolean>) => {
      const deadline = Date.now() + 8000;
      for (;;) {
        await act(async () => {
          await new Promise((resolve) => setTimeout(resolve, 20));
        });
        await screen.renderOnce();
        if (await check()) return;
        if (Date.now() >= deadline)
          throw new Error(`Native UI did not settle:\n${screen.captureCharFrame()}`);
      }
    };
    await act(async () => {
      client.start();
      await new Promise((resolve) => setImmediate(resolve));
    });
    await act(async () => {
      await screen.mockInput.pressKeys(["4"]);
    });
    await act(async () => {
      screen.mockInput.pressEnter();
    });
    await wait(
      () =>
        /NATIVE_TUI_READY/.test(screen.captureCharFrame()) &&
        /INPUT/.test(screen.captureCharFrame()),
    );
    // The agent's window fits the terminal's laid-out width (the 90-column
    // screen less its frame), not the renderable's default 80 columns.
    const width = Number(
      (await tmux(["display-message", "-p", "-t", "agent", "#{window_width}"])).stdout.trim(),
    );
    assert.ok(
      width > 80 && width <= 90,
      `agent window should span the terminal view, got ${width} columns`,
    );
    const paste = `pasted c456 ${"abc🧪".repeat(4000)}END_PASTE`;
    await act(async () => {
      await screen.mockInput.typeText("q123gg");
      screen.mockInput.pressKey("c", { ctrl: true });
      await screen.mockInput.pasteBracketedText(paste);
    });
    await wait(async () => (await readFile(input, "utf8")).includes("END_PASTE"));
    assert.ok(
      (await readFile(input, "utf8")).includes(paste),
      "large Unicode paste is preserved across bounded wire frames",
    );
    assert.equal(exits, 0, "q belongs to the agent while typing");
    assert.deepEqual(calls, [], "native keys must not invoke dashboard operations");
    assert.match(await readFile(input, "utf8"), /q123gg\x03/);
    await act(async () => {
      screen.mockInput.pressKey("]", { ctrl: true });
    });
    await wait(() => /1 agent\b/.test(screen.captureCharFrame()));
    assert.doesNotMatch(await readFile(input, "utf8"), /\x1d/);
    await act(async () => {
      await screen.mockInput.pressKeys(["q"]);
    });
    assert.equal(exits, 1);
    await tmux(["has-session", "-t", "agent"]);
  } finally {
    if (setup)
      await act(async () => {
        client.close();
        await setup!.dispose();
      });
    else client.close();
    await host?.stop();
    // tmux leaves its socket file behind.
    const socketPath = (
      await tmux(["display-message", "-p", "#{socket_path}"]).catch(() => undefined)
    )?.stdout.trim();
    await tmux(["kill-server"]).catch(() => {});
    if (socketPath) await rm(socketPath, { force: true });
    await rm(root, { recursive: true, force: true });
  }
});
