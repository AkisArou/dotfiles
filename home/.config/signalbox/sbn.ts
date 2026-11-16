// sbn: one command for the Signalbox projects under ~/.config/signalbox.
// It finds the project from the current directory (each project's project.json
// names its roots), and runs that project's host unit, TUI, browser and checks.
import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, realpathSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

interface Project {
  readonly name: string;
  readonly directory: string;
  readonly roots: readonly string[];
  readonly webPort: number;
  readonly apiPort: number;
}

const HELP = `sbn [--project <name>] [--api <ws-url>] [command]

  (none) | tui      the terminal UI on the project's running host
  web               open the browser UI
  status            the host unit, and the project's assignments
  start | stop | restart | enable | disable
                    the host unit (systemctl --user ... signalbox@<project>)
  logs              follow the host's log
  doctor            the project's read-only prerequisites
  list              the projects and their hosts

The project is the one whose root contains the current directory, the only
project if there is one, or --project. --api points the TUI at another host.`;

const home = dirname(fileURLToPath(import.meta.url));
const expand = (path: string) => resolve(path.replace(/^~(?=$|\/)/, homedir()));
const real = (path: string) => (existsSync(path) ? realpathSync(path) : path);

function projects(): Project[] {
  return readdirSync(home, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && existsSync(join(home, entry.name, "project.json")))
    .map((entry) => {
      const manifest = JSON.parse(readFileSync(join(home, entry.name, "project.json"), "utf8")) as Omit<
        Project,
        "directory"
      >;
      return { ...manifest, directory: join(home, entry.name), roots: manifest.roots.map((root) => real(expand(root))) };
    });
}

/** The project whose root contains `cwd`, deepest root first. */
export function projectFor(all: readonly Project[], cwd: string): Project | undefined {
  const here = real(cwd);
  const matches = all.flatMap((project) =>
    project.roots
      .filter((root) => here === root || here.startsWith(root.endsWith(sep) ? root : root + sep))
      .map((root) => ({ project, depth: root.length })),
  );
  return matches.sort((a, b) => b.depth - a.depth)[0]?.project;
}

function run(command: string, args: readonly string[], options: { cwd?: string; env?: NodeJS.ProcessEnv } = {}) {
  const result = spawnSync(command, args, { stdio: "inherit", ...options });
  if (result.error) throw result.error;
  return result.status ?? 1;
}

function main(argv: readonly string[]): number {
  const args = [...argv];
  const option = (name: string) => {
    const index = args.indexOf(name);
    if (index < 0) return undefined;
    const [, value] = args.splice(index, 2);
    if (!value) throw new Error(`${name} needs a value`);
    return value;
  };
  if (args.includes("-h") || args.includes("--help")) return console.log(HELP), 0;
  const named = option("--project"),
    api = option("--api");
  const command = args.shift() ?? "tui";
  const all = projects();
  if (command === "list") {
    for (const project of all) {
      const state = spawnSync("systemctl", ["--user", "is-active", `signalbox@${project.name}`], { encoding: "utf8" });
      console.log(`${project.name}\t${state.stdout.trim()}\t${project.roots.join(", ")}\tweb :${project.webPort} api :${project.apiPort}`);
    }
    return 0;
  }
  const project = named
    ? all.find((candidate) => candidate.name === named)
    : (projectFor(all, process.cwd()) ?? (all.length === 1 ? all[0] : undefined));
  if (!project) {
    console.error(
      named
        ? `No project ${named}; projects: ${all.map((candidate) => candidate.name).join(", ")}`
        : `Not inside a project's root; use --project with one of: ${all.map((candidate) => candidate.name).join(", ")}`,
    );
    return 2;
  }
  const unit = `signalbox@${project.name}`;
  const url = api ?? `ws://127.0.0.1:${project.apiPort}/rpc`;
  switch (command) {
    case "tui":
      return run(process.execPath, ["scripts/tui.ts"], {
        cwd: project.directory,
        env: { ...process.env, SIGNALBOX_API_URL: url },
      });
    case "web":
      return run("xdg-open", [`http://127.0.0.1:${project.webPort}`]);
    case "start":
    case "stop":
    case "restart":
    case "enable":
    case "disable":
      return run("systemctl", ["--user", command, unit]);
    case "logs":
      return run("journalctl", ["--user", "-u", unit, "-f", "-n", "100", "-o", "cat"]);
    case "doctor":
      return run(process.execPath, ["scripts/doctor.ts"], { cwd: project.directory });
    case "status": {
      run("systemctl", ["--user", "status", unit, "--no-pager", "--lines", "0"]);
      return run(
        process.execPath,
        [join(project.directory, "node_modules/signalbox/bin/signalbox.js"), "read", "nts.assignments", "--url", url],
        { cwd: project.directory },
      );
    }
    default:
      console.error(`Unknown command ${command}\n\n${HELP}`);
      return 2;
  }
}

if (process.argv[1] && real(process.argv[1]) === real(fileURLToPath(import.meta.url)))
  process.exitCode = main(process.argv.slice(2));
