import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";
import { createTerminalTools } from "./tools.ts";

function terminal() {
  const events: string[] = [];
  return {
    events,
    isDestroyed: false,
    suspend: async () => {
      events.push("suspend");
    },
    resume: async () => {
      events.push("resume");
    },
  };
}
test("finder receives exact candidate lines, inherits the environment, and restores the terminal after errors and cancellation", async () => {
  const display = terminal();
  const tools = createTerminalTools(display, (title) => ({
    command: process.execPath,
    args: [
      "-e",
      "const fs=require('node:fs'); if(process.argv[1]!=='Find Tasks') process.exit(2); process.stdout.write(fs.readFileSync(0,'utf8').trimEnd().split('\\n')[1]+'\\n');",
      title,
    ],
  }));
  assert.equal(await tools.find(["compiler", "runtime"], "Find Tasks"), 1);
  assert.deepEqual(display.events, ["suspend", "resume"]);
  await assert.rejects(
    tools.run({ command: "signalbox-missing-test-tool" }, tmpdir()),
    /not installed/,
  );
  assert.equal(display.events.at(-1), "resume");
  const cancel = createTerminalTools(display, () => ({
    command: process.execPath,
    args: ["-e", "process.exit(130)"],
  }));
  assert.equal(await cancel.find(["one"], "cancel"), undefined);
  await cancel.close();
  await tools.close();
});

test("local tools run in the selected directory with argv and environment intact", async () => {
  const directory = await mkdtemp(join(tmpdir(), "signalbox-tool-"));
  const tools = createTerminalTools(terminal(), () => ({ command: "unused" }));
  const literal = "literal $() `value` with spaces";
  try {
    await tools.run(
      {
        command: process.execPath,
        args: [
          "-e",
          "require('node:fs').writeFileSync('received.json',JSON.stringify({cwd:process.cwd(),arg:process.argv[1],path:process.env.PATH}))",
          literal,
        ],
      },
      directory,
    );
    const result = JSON.parse(await readFile(join(directory, "received.json"), "utf8"));
    assert.equal(result.cwd, directory);
    assert.equal(result.arg, literal);
    assert.equal(result.path, process.env.PATH);
  } finally {
    await tools.close();
    await rm(directory, { recursive: true, force: true });
  }
});

test("closing the viewer cancels an active finder and restores terminal ownership", async () => {
  const display = terminal();
  const tools = createTerminalTools(display, () => ({
    command: process.execPath,
    args: ["-e", "setInterval(()=>{},1000)"],
  }));
  const finding = assert.rejects(tools.find(["one"], "wait"), /SIGTERM|closed/);
  await new Promise((resolve) => setTimeout(resolve, 20));
  await tools.close();
  await finding;
  assert.deepEqual(display.events, ["suspend", "resume"]);
  await assert.rejects(tools.find(["one"], "closed"), /closed/);
});

test(
  "installed fzf selects by fuzzy matching without requiring tmux",
  { skip: spawnSync("fzf", ["--version"]).status !== 0 },
  async () => {
    const display = terminal();
    const tools = createTerminalTools(display, () => ({
      command: "fzf",
      args: ["--filter=nmsp", "--no-multi", "--no-print-query", "--no-expect"],
    }));
    try {
      assert.equal(await tools.find(["negative indices", "numeric separators"], "find"), 1);
    } finally {
      await tools.close();
    }
  },
);
