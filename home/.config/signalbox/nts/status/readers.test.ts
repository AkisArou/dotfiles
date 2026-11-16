import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, readFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { liveReaders } from "./readers.ts";

test("live NTS readers execute status commands, reject changed formats and support cancellation", async () => {
  const root = await mkdtemp(join(tmpdir(), "signalbox-nts-readers-"));
  try {
    await mkdir(join(root, "tooling/records"), { recursive: true });
    const fixture = { version: 2, kind: "benchmarks", run: null, counts: null, rows: [] };
    await writeFile(join(root, "benchmarks.json"), JSON.stringify(fixture));
    await writeFile(
      join(root, "conformance.json"),
      JSON.stringify({ version: 1, kind: "conformance", backends: { c: {}, llvm: {}, jvm: {} } }),
    );
    await writeFile(
      join(root, "tooling/records/summary.ts"),
      `import {readFileSync,appendFileSync} from 'node:fs'; const kind=process.argv[2]; appendFileSync('commands.jsonl',JSON.stringify(process.argv.slice(2))+'\\n'); console.log(readFileSync(kind+'.json','utf8'));`,
    );
    const abort = new AbortController(),
      readers = liveReaders(root, abort.signal);
    assert.ok((await readers.Benchmarks()).tables.some((table) => table.id === "bench-run"));
    assert.ok(
      (await readers.Conformance()).tables.some((table) => table.id === "conformance-suite"),
    );
    assert.deepEqual(
      (await readFile(join(root, "commands.jsonl"), "utf8"))
        .trim()
        .split("\n")
        .map((line) => JSON.parse(line)),
      [["benchmarks"], ["conformance"]],
    );
    await writeFile(join(root, "benchmarks.json"), JSON.stringify({ ...fixture, version: 999 }));
    await assert.rejects(readers.Benchmarks(), /unsupported/);
    abort.abort();
    await assert.rejects(readers.Benchmarks(), (error) => (error as Error).name === "AbortError");
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
