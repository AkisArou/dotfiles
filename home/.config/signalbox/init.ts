// What sbn init writes: a git project's folder (.signalbox/ in its repository), the
// kit's default assembly (@signalbox/kit/project/git) configured by its project.json.
// Pure: sbn.ts reads the repository and writes these files.

export type InitOptions = {
  readonly name: string;
  readonly title: string;
  /** The checkout, as written to project.json (~ kept). */
  readonly root: string;
  /** Where task trees go (~/.cache/<name>-trees). */
  readonly trees: string;
  readonly apiPort: number;
  readonly remote: string;
  readonly branch: string;
  readonly tasks: "local" | { readonly github: string };
  readonly setup?: string;
  readonly check?: string;
  /** The SDK checkout the project's packages come from. */
  readonly sdk: string;
};

/** A project name from a folder's: lower case, letters, digits and -. */
export function projectName(folder: string): string {
  return folder
    .toLowerCase()
    .replace(/[^a-z0-9-]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

/** Its title: the name with a capital (zync: Zync; my-app: My app). */
export const projectTitle = (name: string) =>
  name.charAt(0).toUpperCase() + name.slice(1).replace(/-/g, " ");

/** The next host port after the others' (nts 4311, nable 4321: 4331). */
export function nextApiPort(taken: readonly number[]): number {
  return taken.length ? Math.max(...taken) + 10 : 4311;
}

/** The install a new task tree needs, from the lockfile at the repository's root. */
export function setupFor(files: readonly string[]): string | undefined {
  const has = (name: string) => files.includes(name);
  if (has("pnpm-lock.yaml"))
    return "pnpm install --frozen-lockfile --prefer-offline";
  if (has("bun.lock") || has("bun.lockb")) return "bun install --frozen-lockfile";
  if (has("yarn.lock")) return "yarn install --immutable";
  if (has("package-lock.json")) return "npm ci";
  return undefined;
}

/** owner/name from a GitHub remote URL (https or ssh), or undefined. */
export function gitHubRepo(url: string): string | undefined {
  const match =
    /^(?:https:\/\/github\.com\/|git@github\.com:|ssh:\/\/git@github\.com\/)([\w.-]+\/[\w.-]+?)(?:\.git)?\/?$/.exec(
      url.trim(),
    );
  return match?.[1];
}

const json = (value: unknown) => `${JSON.stringify(value, null, 2)}\n`;

/** The project folder's files, by path inside it. */
export function scaffold(options: InitOptions): Record<string, string> {
  const sdk = (name: string) => `file:${options.sdk}/${name}`;
  return {
    "project.json": json({
      name: options.name,
      title: options.title,
      roots: [options.root, options.trees],
      apiPort: options.apiPort,
      coordinator: false,
      remote: options.remote,
      branch: options.branch,
      tasks: options.tasks,
      ...(options.setup ? { setup: options.setup } : {}),
      ...(options.check ? { check: options.check } : {}),
    }),
    // One worker to start, on Codex; Claude Code reviews and coordinates.
    "settings.json": json({
      workerSlots: 1,
      maxRounds: 4,
      roles: {
        coordinator: {
          pick: "first",
          models: [
            { harness: "claude", model: "claude-opus-5-5", effort: "high" },
          ],
        },
        worker: { pick: "first", models: [{ harness: "codex", effort: "high" }] },
        reviewer: {
          pick: "first",
          models: [
            { harness: "claude", model: "claude-opus-5-5", effort: "high" },
          ],
        },
      },
    }),
    "package.json": json({
      name: `signalbox-${options.name}`,
      version: "0.0.0",
      private: true,
      type: "module",
      description: `${options.title} on the Signalbox SDK: the kit's git project`,
      engines: { node: ">=26.4.0" },
      scripts: {
        start: "node server.ts",
        doctor: "node scripts/doctor.ts",
        tui: "node scripts/tui.ts",
        check: "tsc -p tsconfig.json && tsc -p tsconfig.tui.json",
      },
      dependencies: {
        "@astryxdesign/core": "0.7.16",
        "@astryxdesign/theme-neutral": "0.7.16",
        "@opentui/core": "0.9.0",
        "@opentui/react": "0.9.0",
        "@signalbox/kit": sdk("kit"),
        "@stylexjs/stylex": "0.19.2",
        "@tanstack/react-router": "1.170.46",
        "@xterm/addon-fit": "^0.11.0",
        "@xterm/xterm": "^6.0.0",
        react: "19.3.1",
        "react-devtools-core": "^7.0.1",
        "react-dom": "19.3.1",
        signalbox: sdk("signalbox"),
        zod: "^4.3.6",
      },
      dependenciesMeta: {
        signalbox: { injected: true },
        "@signalbox/kit": { injected: true },
      },
      devDependencies: {
        "@types/node": "^24.19.2",
        "@types/react": "19.3.0",
        "@types/react-dom": "^19.3.0",
        esbuild: "^0.28.2",
        typescript: "7.0.2",
      },
    }),
    ".gitignore": "node_modules/\n",
    "README.md": `# ${options.title} on Signalbox

Made by \`sbn init\`. The kit's git project (\`@signalbox/kit/project/git\`) runs it;
\`project.json\` says everything it needs, and \`settings.json\` its workers and models.

| project.json   | What                                                                 |
| -------------- | -------------------------------------------------------------------- |
| \`tasks\`        | \`"local"\`: this machine's board (\`sbn task ...\`), or \`{ "github": "owner/name" }\` |
| \`setup\`        | run in each new task tree and before the check (an install)          |
| \`check\`        | what a reviewed change must pass before it is pushed; none lands it as reviewed |
| \`coordinator\`  | \`true\` runs the coordinator; \`false\`, you assign tasks (\`sbn assign <n>\`) |
| \`instructions\` | the repository's rules for its agents (\`AGENTS.md\`)                  |

    sbn task add <title...> [--body <text>]   a task on the board
    sbn assign <n>                            a worker takes it
    sbn                                       the terminal app; sbn web, the console
    sbn doctor                                what this machine still lacks
`,
    "server.ts": `// ${options.title}'s host: the kit's git project, configured by project.json.
import { serveGitProject } from "@signalbox/kit/project/git";

await serveGitProject(new URL("./", import.meta.url));
`,
    "scripts/cli.ts": `// ${options.title}'s verbs for sbn: the kit's, and the task board's (sbn task ...).
import { gitProjectCli } from "@signalbox/kit/project/cli";

process.exitCode = await gitProjectCli(process.argv.slice(2), new URL("../", import.meta.url));
`,
    "scripts/doctor.ts": `// What this machine still lacks for ${options.title} (sbn doctor), read-only.
import { gitProjectDoctor } from "@signalbox/kit/project/doctor";

process.exitCode = await gitProjectDoctor(new URL("../", import.meta.url));
`,
    "scripts/tui.ts": `// ${options.title}'s terminal app, launched by the kit.
import { fileURLToPath } from "node:url";
import { launchTerminalApp } from "@signalbox/kit/tui/launch";

process.exitCode = await launchTerminalApp({
  directory: fileURLToPath(new URL("../", import.meta.url)),
  testing: process.argv.includes("--test"),
});
`,
    "tui/main.tsx": `/** @jsxImportSource @opentui/react */
// ${options.title}'s terminal app: the kit's pages.
import { useProject } from "@signalbox/kit/shared/use-project";
import { agentsTuiPage, needsYouTuiPage, nowTuiPage, tasksTuiPage } from "@signalbox/kit/tui/pages";
import type { TuiApp } from "@signalbox/kit/tui/pages";
import { runTerminalApp } from "@signalbox/kit/tui/run";

type Project = ReturnType<typeof useProject>;
const app: TuiApp<Project> = {
  name: ${JSON.stringify(options.name)},
  useProject,
  pages: [nowTuiPage<Project>(), needsYouTuiPage, tasksTuiPage, agentsTuiPage],
};

// scripts/tui.ts (the kit's launcher) passes this project's host, or sbn --api's.
const api = process.env.SIGNALBOX_API_URL;
if (!api) throw new Error("SIGNALBOX_API_URL names the host; start the TUI with sbn");
await runTerminalApp(app, { api });
`,
    "web/console.tsx": `// ${options.title}'s pages in the Signalbox console: the kit's standard ones.
import { useProject } from "@signalbox/kit/shared/use-project";
import { consoleProject } from "@signalbox/kit/web/console";
import { agentsPage, needsYouPage, nowPage, tasksPage } from "@signalbox/kit/web/standard-pages";

type Project = ReturnType<typeof useProject>;
export default consoleProject<Project>({
  useProject,
  pages: [nowPage<Project>(), needsYouPage, tasksPage, agentsPage],
});
`,
    "tsconfig.json": json({
      compilerOptions: {
        target: "ES2024",
        module: "ESNext",
        moduleResolution: "bundler",
        strict: true,
        noUncheckedIndexedAccess: true,
        verbatimModuleSyntax: true,
        allowImportingTsExtensions: true,
        noEmit: true,
        skipLibCheck: true,
        jsx: "react-jsx",
        types: ["node", "react", "react-dom"],
        lib: ["ES2024", "DOM"],
      },
      include: ["server.ts", "web/**/*.tsx", "scripts/*.ts"],
      exclude: ["node_modules"],
    }),
    "tsconfig.tui.json": json({
      extends: "./tsconfig.json",
      compilerOptions: { jsxImportSource: "@opentui/react", types: ["node", "react"] },
      include: ["tui/**/*.tsx", "scripts/**/*.ts"],
      exclude: ["node_modules"],
    }),
  };
}
