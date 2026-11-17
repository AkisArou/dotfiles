// sbn: one command for the Signalbox projects under ~/.config/signalbox.
// It finds the project from the current directory (each project's project.json
// names its roots), and runs that project's host unit, TUI, browser and checks.
import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, realpathSync } from "node:fs";
import { homedir, userInfo } from "node:os";
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
  status            the host unit, and the project's own status
  start | stop | restart | enable | disable
                    the host unit (systemctl --user ... signalbox@<project>)
  logs              follow the host's log
  tailnet [off]     serve the browser UI on your tailnet (tailscale serve), or stop
  doctor            the project's read-only prerequisites
  list              the projects and their hosts
  <verb> ...        any other verb goes to the project's scripts/cli.ts
                    (nts: show, assign, approve, return, message, rework, retry, cancel)

The project is the one whose root contains the current directory, the only
project if there is one, or --project. --api points the TUI and the project's
verbs at another host.`;

const home = dirname(fileURLToPath(import.meta.url));
const expand = (path: string) => resolve(path.replace(/^~(?=$|\/)/, homedir()));
const real = (path: string) => (existsSync(path) ? realpathSync(path) : path);

function projects(): Project[] {
  return readdirSync(home, { withFileTypes: true })
    .filter(
      (entry) =>
        entry.isDirectory() &&
        existsSync(join(home, entry.name, "project.json")),
    )
    .map((entry) => {
      const manifest = JSON.parse(
        readFileSync(join(home, entry.name, "project.json"), "utf8"),
      ) as Omit<Project, "directory">;
      return {
        ...manifest,
        directory: join(home, entry.name),
        roots: manifest.roots.map((root) => real(expand(root))),
      };
    });
}

/** The project whose root contains `cwd`, deepest root first. */
export function projectFor(
  all: readonly Project[],
  cwd: string,
): Project | undefined {
  const here = real(cwd);
  const matches = all.flatMap((project) =>
    project.roots
      .filter(
        (root) =>
          here === root ||
          here.startsWith(root.endsWith(sep) ? root : root + sep),
      )
      .map((root) => ({ project, depth: root.length })),
  );
  return matches.sort((a, b) => b.depth - a.depth)[0]?.project;
}

/** The browser UI's routes on a tailnet: the page, its API and its terminals. */
export function serveRoutes(project: { webPort: number; apiPort: number }) {
  return [
    { path: "/", target: `http://127.0.0.1:${project.webPort}` },
    { path: "/rpc", target: `http://127.0.0.1:${project.apiPort}/rpc` },
    {
      path: "/terminals",
      target: `http://127.0.0.1:${project.apiPort}/terminals`,
    },
  ];
}
/** tailscale serve's arguments to add this project's routes, or (off) to remove only them. */
export function serveCommands(
  project: { webPort: number; apiPort: number },
  on: boolean,
) {
  return serveRoutes(project).map((route) =>
    on
      ? ["serve", "--bg", "--set-path", route.path, route.target]
      : ["serve", "--https=443", "--set-path", route.path, "off"],
  );
}

function run(
  command: string,
  args: readonly string[],
  options: { cwd?: string; env?: NodeJS.ProcessEnv } = {},
) {
  const result = spawnSync(command, args, { stdio: "inherit", ...options });
  if (result.error) throw result.error;
  return result.status ?? 1;
}

/** A project's own verbs (scripts/cli.ts), if it has any. */
function projectVerb(
  project: Project,
  args: readonly string[],
  url: string,
): number | undefined {
  const cli = join(project.directory, "scripts/cli.ts");
  if (!existsSync(cli)) return undefined;
  return run(process.execPath, [cli, ...args], {
    cwd: project.directory,
    env: { ...process.env, SIGNALBOX_API_URL: url },
  });
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
  if (args.includes("-h") || args.includes("--help"))
    return (console.log(HELP), 0);
  const named = option("--project"),
    api = option("--api");
  const command = args.shift() ?? "tui";
  const all = projects();
  if (command === "list") {
    for (const project of all) {
      const state = spawnSync(
        "systemctl",
        ["--user", "is-active", `signalbox@${project.name}`],
        { encoding: "utf8" },
      );
      console.log(
        `${project.name}\t${state.stdout.trim()}\t${project.roots.join(", ")}\tweb :${project.webPort} api :${project.apiPort}`,
      );
    }
    return 0;
  }
  const project = named
    ? all.find((candidate) => candidate.name === named)
    : (projectFor(all, process.cwd()) ??
      (all.length === 1 ? all[0] : undefined));
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
    case "web": {
      const page = `http://127.0.0.1:${project.webPort}`;
      if (process.env.DISPLAY || process.env.WAYLAND_DISPLAY)
        return run("xdg-open", [page]);
      // No display here (an SSH session): the browser is on the other end of the connection.
      const here = process.env.SSH_CONNECTION?.split(" ")[2] ?? "this-machine";
      console.log(
        `No display here. On the computer you connect from, forward the two ports:\n\n` +
          `  ssh -N -L ${project.webPort}:127.0.0.1:${project.webPort} -L ${project.apiPort}:127.0.0.1:${project.apiPort} ${userInfo().username}@${here}\n\n` +
          `then open http://localhost:${project.webPort} there. The forward lasts while that ssh runs.\n` +
          `Or, on your tailnet (a phone too): sbn tailnet.`,
      );
      return 0;
    }
    case "start":
    case "stop":
    case "restart":
      return run("systemctl", ["--user", command, unit]);
    // At login from now on, and now.
    case "enable":
    case "disable":
      return run("systemctl", ["--user", command, "--now", unit]);
    case "logs":
      return run("journalctl", [
        "--user",
        "-u",
        unit,
        "-f",
        "-n",
        "100",
        "-o",
        "cat",
      ]);
    // The browser UI on your tailnet, for a phone or another machine: Tailscale's own
    // HTTPS address, proxied to this host, which accepts it (it reads the address too).
    case "tailnet": {
      const on = args[0] !== "off";
      for (const serve of serveCommands(project, on)) {
        const code = run("tailscale", serve);
        if (code !== 0) return code;
      }
      if (!on) return 0;
      const status = spawnSync("tailscale", ["status", "--json"], {
        encoding: "utf8",
      });
      const name = (
        JSON.parse(status.stdout || "{}") as { Self?: { DNSName?: string } }
      ).Self?.DNSName;
      console.log(
        name
          ? `\nOpen https://${name.replace(/\.$/, "")} on a device in your tailnet.`
          : "\nShared; tailscale status names this machine's address.",
      );
      return 0;
    }
    case "doctor":
      return run(process.execPath, ["scripts/doctor.ts"], {
        cwd: project.directory,
      });
    case "status": {
      const state = spawnSync("systemctl", ["--user", "is-active", unit], {
        encoding: "utf8",
      });
      console.log(
        `${unit}: ${state.stdout.trim()}${api ? ` (showing ${url})` : ""}`,
      );
      return projectVerb(project, ["status"], url) ?? 0;
    }
    default: {
      const code = projectVerb(project, [command, ...args], url);
      if (code !== undefined) return code;
      console.error(`Unknown command ${command}\n\n${HELP}`);
      return 2;
    }
  }
}

if (
  process.argv[1] &&
  real(process.argv[1]) === real(fileURLToPath(import.meta.url))
)
  process.exitCode = main(process.argv.slice(2));
