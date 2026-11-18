// The nts host: the kit's, reading nts's issues from GitHub with its plan,
// its label meanings from .github/labels.json, its orchestration, and its status
// pages' readings (status/host.ts). nts's tools keep owning its trees and landing.
import { readFile, stat } from "node:fs/promises";
import { join } from "node:path";
import { projectHost } from "@signalbox/kit/host/app";
import { discoverTerminals } from "@signalbox/kit/host/discovery";
import type { NtsConfig } from "../config.ts";
import { ntsObservations } from "../status/host.ts";
import { projectIssues, readIssues } from "./tasks.ts";
import { ntsOrchestration } from "../orchestration/index.ts";
import { gitHub } from "../orchestration/github.ts";

/** The nts host; `origins` are the console's pages that may call it. */
export function createNtsProject(
  settings: NtsConfig,
  served: { readonly origins: () => readonly string[] },
) {
  const planPath = join(settings.root, settings.plan);
  const plan = () => readFile(planPath, "utf8");
  return projectHost({
    name: settings.name,
    title: settings.title,
    root: settings.root,
    repo: settings.repo,
    state: settings.state,
    apiPort: settings.apiPort,
    origins: served.origins,
    issues: {
      intervalMs: settings.issueIntervalMs,
      async read(signal) {
        const listed = await readIssues(settings.repo, settings.issueLimit, signal);
        // The plan has its own failure reporting; issue visibility doesn't depend on it.
        const text = await plan().catch((error: NodeJS.ErrnoException) => {
          if (error.code === "ENOENT") return "";
          throw error;
        });
        return projectIssues(listed, text);
      },
    },
    plan,
    // What each label means, from the project's own vocabulary file.
    async labels() {
      const file = JSON.parse(
        await readFile(join(settings.root, ".github/labels.json"), "utf8"),
      ) as {
        labels?: { name: string; description?: string }[];
      };
      return Object.fromEntries(
        (file.labels ?? []).map((label) => [label.name, label.description ?? ""]),
      );
    },
    terminals: {
      programs: settings.terminalPrograms,
      attached: settings.attachedTerminals,
      // Agents the user runs in their own tmux, in the checkout or a pool tree.
      ...(settings.discoverTerminals
        ? { discover: (signal) => discoverTerminals([settings.root, settings.poolRoot], signal) }
        : {}),
    },
    orchestration: (inputs) =>
      ntsOrchestration({
        project: settings.name,
        root: settings.root,
        directory: settings.state,
        ...inputs,
        settings: settings.orchestration,
        gitHub: gitHub(settings.repo),
      }),
    extensions: (orchestration) => [
      ntsObservations({
        root: settings.root,
        landing: orchestration.landing,
        fullCheck: orchestration.fullCheck,
      }),
    ],
  });
}

export async function requireCheckout(settings: NtsConfig) {
  if (!(await stat(settings.root).catch(() => undefined))?.isDirectory())
    throw new Error(
      `NTS checkout not found: ${settings.root}. Set NTS_ROOT to your existing checkout; run pnpm --dir playground run doctor for prerequisites.`,
    );
  if (
    !(await stat(join(settings.root, "tooling/landing/check.ts")).catch(() => undefined))?.isFile()
  )
    throw new Error(`Not an NTS checkout: ${settings.root} (missing tooling/landing/check.ts)`);
}
