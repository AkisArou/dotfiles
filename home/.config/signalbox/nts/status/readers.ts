// USER READERS: only NTS status commands. No benchmarks, builds or services are started.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { readFile, stat } from "node:fs/promises";
import { benchmarkReport, conformanceCommits, conformanceReport } from "./records.ts";
import { benchmarkHistory } from "./bench-history.ts";
import type { BenchmarksSummary, ConformanceSummary } from "./records.ts";
import { machineReport } from "./machine.ts";
import type { LedgerStatus } from "./machine.ts";
import { poolStatus } from "./commands.ts";
import type { NtsPage, Report, TableCell } from "./model.ts";
import { checked } from "./status.ts";

const execute = promisify(execFile);
/** Pages read on their own; Landing and Full check project the orchestration's sources. */
export type ReadPage = Exclude<NtsPage, "Landing" | "Full check">;
const intervals: Record<ReadPage, number> = {
  Benchmarks: 300_000,
  Conformance: 300_000,
  Machine: 15_000,
};
export { intervals };
export function liveReaders(
  root: string,
  signal: AbortSignal,
): Record<ReadPage, () => Promise<Report>> {
  async function run(command: string, args: readonly string[], timeout = 30_000) {
    const result = await execute(command, [...args], {
      cwd: root,
      signal,
      timeout,
      maxBuffer: 16 * 1024 * 1024,
    });
    return result.stdout;
  }
  const json = async (args: readonly string[]) =>
    JSON.parse(await run(process.execPath, args, 60_000)) as unknown;
  return {
    async Benchmarks() {
      return benchmarkReport(
        checked<BenchmarksSummary>(
          await json(["tooling/records/summary.ts", "benchmarks"]),
          "benchmarks",
          // Version 2 added fields this projection does not read (references, ownBest).
          [2],
          "benchmarks",
        ),
        Date.now(),
        // Progress across the newest complete runs (each run file read once).
        await benchmarkHistory(root),
      );
    },
    async Conformance() {
      const summary = checked<ConformanceSummary>(
        await json(["tooling/records/summary.ts", "conformance"]),
        "conformance",
        [1],
        "conformance",
      );
      const behind = new Map<string, number>();
      // Bound process concurrency; record files can reference many different commits.
      for (const commit of conformanceCommits(summary)) {
        if (!/^[a-f0-9]{7,64}$/i.test(commit))
          throw new Error(`Invalid NTS record commit: ${commit}`);
        const count = Number(
          (
            await run("git", [
              "rev-list",
              "--count",
              `${commit}..main`,
              "--",
              "compiler",
              "runtime",
            ])
          ).trim(),
        );
        if (!Number.isSafeInteger(count) || count < 0)
          throw new Error(`Invalid compiler commit count for ${commit}`);
        behind.set(commit, count);
      }
      return conformanceReport(summary, behind);
    },
    async Machine() {
      const pool = await poolStatus(root, signal);
      const ledger = checked<LedgerStatus>(
        await json(["tooling/gate/admit.ts", "--status", "--json"]),
        "memory",
        [1],
        "memory",
      );
      const listed = await run(
        "systemctl",
        ["--user", "list-units", "nts-*", "--type=service", "--all", "--no-legend", "--plain"],
        10_000,
      );
      const units = [
        ...new Set([
          "nts-landing-check.service",
          "nts-full-check.service",
          ...listed
            .split("\n")
            .map((line) => line.trim().split(/\s+/)[0] ?? "")
            .filter((unit) => unit.endsWith(".service")),
        ]),
      ];
      const properties = await run(
        "systemctl",
        [
          "--user",
          "show",
          "--timestamp=unix",
          "-p",
          "Id,ActiveState,SubState,MemoryCurrent,CPUUsageNSec,StateChangeTimestamp",
          ...units,
        ],
        10_000,
      );
      let oomKills: number | undefined;
      try {
        const log = await run(
          "journalctl",
          [
            "-k",
            "--since",
            "-24h",
            "-q",
            "--no-pager",
            "-g",
            "oom-kill:constraint=CONSTRAINT_NONE",
          ],
          10_000,
        );
        oomKills = log.split("\n").filter((line) => line.includes("oom-kill:")).length;
      } catch (error) {
        signal.throwIfAborted();
        const failure = error as { code?: unknown; stderr?: string };
        if (failure.code === 1 && !failure.stderr?.trim()) oomKills = 0;
      }
      return machineReport({
        pool,
        ledger,
        units: properties,
        oomKills,
        load: (await readFile("/proc/loadavg", "utf8")).split(" ").slice(0, 3).join(" "),
        measurementLock: await measurementLock(),
      });
    },
  };
}

async function measurementLock(): Promise<TableCell> {
  const path = process.env.NTS_MEASUREMENT_LOCK ?? "/tmp/nts-gate/gate.lock.d";
  let pid: string;
  try {
    pid = (await readFile(`${path}/pid`, "utf8")).trim();
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    const exists = await stat(path).then(
      () => true,
      () => false,
    );
    return {
      text: exists ? "being taken; no pid yet" : "free",
      color: exists ? "yellow" : "green",
    };
  }
  if (!/^\d+$/.test(pid)) return { text: "measurement lock has an invalid pid", color: "yellow" };
  try {
    const command = (await readFile(`/proc/${pid}/cmdline`, "utf8"))
      .split("\0")
      .filter(Boolean)
      .join(" ");
    return { text: `held by ${pid}: ${command}`, color: "yellow" };
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    return { text: `stale lock: pid ${pid} is gone`, color: "red" };
  }
}
