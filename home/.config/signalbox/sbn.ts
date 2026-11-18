// sbn: one command for the Signalbox projects under ~/.config/signalbox.
// It finds the project from the current directory (each project's project.json
// names its roots), and runs that project's host unit, TUI and checks, and the
// console (every project's web pages and notifications, console/).
import { spawnSync } from "node:child_process";
import {
  existsSync,
  lstatSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { homedir, hostname, userInfo } from "node:os";
import { dirname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

interface Project {
  readonly name: string;
  readonly directory: string;
  readonly roots: readonly string[];
  readonly apiPort: number;
}

const HELP = `sbn [--project <name>] [--api <ws-url>] [command]

  (none) | tui      the terminal UI on the project's running host
  web               open the project in the console (the browser UI)
  status            the host unit, and the project's own status
  start | stop | restart | enable | disable
                    the host unit (systemctl --user ... signalbox@<project>)
  logs              follow the host's log
  tailnet [off]     serve the console and this machine's hosts on your tailnet
                    (tailscale serve), or stop
  doctor            the project's read-only prerequisites
  list              the projects and their hosts
  hosts             which machine runs which project's host (hosts.json, by hostname)
  hosts sync        start and enable this machine's hosts and the console; stop and
                    disable the others
  links             link the project folders that live in other repositories
                    (links.json: name -> folder) into this directory
  hosts here <project...>
                    make those this machine's hosts, then sync
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

/** The console's ports on this machine (console/console.json). */
export type ConsolePorts = {
  readonly webPort: number;
  readonly apiPort: number;
};
const consolePorts = (): ConsolePorts =>
  JSON.parse(
    readFileSync(join(home, "console/console.json"), "utf8"),
  ) as ConsolePorts;

/**
 * This machine's routes on its tailnet address: the console (its page at /, its API
 * at /console/rpc), and each host it runs at /<name>/rpc and /<name>/terminals.
 */
export function serveRoutes(
  ports: ConsolePorts,
  hosts: readonly { readonly name: string; readonly apiPort: number }[],
) {
  return [
    { path: "/", target: `http://127.0.0.1:${ports.webPort}` },
    { path: "/console/rpc", target: `http://127.0.0.1:${ports.apiPort}/rpc` },
    ...hosts.flatMap((host) => [
      {
        path: `/${host.name}/rpc`,
        target: `http://127.0.0.1:${host.apiPort}/rpc`,
      },
      {
        path: `/${host.name}/terminals`,
        target: `http://127.0.0.1:${host.apiPort}/terminals`,
      },
    ]),
  ];
}
/** tailscale serve's arguments to add these routes, or (off) to remove only them. */
export function serveCommands(
  routes: readonly { path: string; target: string }[],
  on: boolean,
) {
  return routes.map((route) =>
    on
      ? ["serve", "--bg", "--set-path", route.path, route.target]
      : ["serve", "--https=443", "--set-path", route.path, "off"],
  );
}

/** Projects whose folder lives in another repository (links.json: name -> folder):
 * each is linked in here, so it joins this workspace beside the others. */
export type Links = Record<string, string>;
export function parseLinks(text: string): Links {
  const value: unknown = JSON.parse(text);
  if (
    !value ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    !Object.values(value).every(
      (folder) => typeof folder === "string" && folder.length > 0,
    )
  )
    throw new Error("links.json maps each project name to its folder");
  return value as Links;
}
/** What linking needs here: links to make, links to repoint, and what cannot be linked. */
export function linkPlan(
  links: Links,
  here: (
    name: string,
  ) => { kind: "missing" } | { kind: "link"; to: string } | { kind: "other" },
  exists: (folder: string) => boolean,
): {
  make: [string, string][];
  repoint: [string, string][];
  absent: [string, string][];
  blocked: string[];
} {
  const plan = {
    make: [] as [string, string][],
    repoint: [] as [string, string][],
    absent: [] as [string, string][],
    blocked: [] as string[],
  };
  for (const [name, folder] of Object.entries(links)) {
    const target = expand(folder);
    const current = here(name);
    if (!exists(target)) plan.absent.push([name, target]);
    else if (current.kind === "missing") plan.make.push([name, target]);
    else if (current.kind === "other") plan.blocked.push(name);
    else if (current.to !== target) plan.repoint.push([name, target]);
  }
  return plan;
}
function linksCommand(): number {
  const path = join(home, "links.json");
  const links = existsSync(path) ? parseLinks(readFileSync(path, "utf8")) : {};
  const plan = linkPlan(
    links,
    (name) => {
      const at = join(home, name);
      try {
        const stat = lstatSync(at);
        return stat.isSymbolicLink()
          ? { kind: "link", to: resolve(home, readlinkSync(at)) }
          : { kind: "other" };
      } catch {
        return { kind: "missing" };
      }
    },
    existsSync,
  );
  for (const [name, target] of [...plan.make, ...plan.repoint]) {
    rmSync(join(home, name), { force: true });
    symlinkSync(target, join(home, name));
    console.log(`linked ${name} -> ${target}`);
  }
  for (const [name, target] of plan.absent)
    console.log(
      `${name}: ${target} is not on this machine (clone its repository, then sbn links)`,
    );
  for (const name of plan.blocked)
    console.error(
      `${name}: ${join(home, name)} is a folder, not a link; move it away to link ${links[name]}`,
    );
  if (
    !plan.make.length &&
    !plan.repoint.length &&
    !plan.absent.length &&
    !plan.blocked.length
  )
    console.log("every linked project is in place");
  return plan.blocked.length ? 1 : 0;
}

/** Which machine runs which projects' hosts: hosts.json, by hostname, in the dotfiles.
 * A hostname is a label the owner chooses (hostnamectl): keep it generic and unique. */
export type Hosts = Record<string, string[]>;
const hostsPath = () => join(home, "hosts.json");
export function parseHosts(text: string): Hosts {
  const value: unknown = JSON.parse(text);
  if (
    !value ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    !Object.values(value).every(
      (projects) =>
        Array.isArray(projects) &&
        projects.every((name) => typeof name === "string"),
    )
  )
    throw new Error("hosts.json maps each hostname to a list of project names");
  return value as Hosts;
}
const readHosts = (): Hosts =>
  existsSync(hostsPath()) ? parseHosts(readFileSync(hostsPath(), "utf8")) : {};
function writeHosts(hosts: Hosts): void {
  const sorted = Object.fromEntries(
    Object.keys(hosts)
      .sort()
      .map((name) => [name, [...new Set(hosts[name])].sort()]),
  );
  writeFileSync(hostsPath(), `${JSON.stringify(sorted, null, 2)}\n`);
}

/** What makes this machine run exactly `wanted`: hosts to start, and running or enabled ones to stop. */
export function hostPlan(
  wanted: readonly string[],
  current: {
    readonly enabled: readonly string[];
    readonly active: readonly string[];
  },
  known: readonly string[],
): { enable: string[]; disable: string[]; unknown: string[] } {
  const unknown = wanted.filter((name) => !known.includes(name));
  const want = wanted.filter((name) => known.includes(name));
  const running = new Set([...current.enabled, ...current.active]);
  return {
    enable: want.filter(
      (name) =>
        !current.enabled.includes(name) || !current.active.includes(name),
    ),
    disable: [...running].filter((name) => !want.includes(name)).sort(),
    unknown,
  };
}

/** This machine's signalbox host instances, enabled or running. */
function hostUnits(): { enabled: string[]; active: string[] } {
  const names = (args: string[]) =>
    spawnSync(
      "systemctl",
      ["--user", ...args, "signalbox@*.service", "--no-legend", "--plain"],
      {
        encoding: "utf8",
      },
    )
      .stdout.split("\n")
      .map((line) => /^signalbox@([^.\s]+)\.service/.exec(line.trim())?.[1])
      .filter((name): name is string => Boolean(name));
  return {
    enabled: names(["list-unit-files", "--state=enabled"]),
    active: names(["list-units", "--state=active"]),
  };
}

function hostsCommand(
  args: readonly string[],
  all: readonly Project[],
): number {
  const machine = hostname();
  const known = all.map((project) => project.name);
  const [verb, ...rest] = args;
  if (verb === "here") {
    if (!rest.length)
      throw new Error("hosts here needs the projects this machine runs");
    const unknown = rest.filter((name) => !known.includes(name));
    if (unknown.length)
      throw new Error(
        `No project ${unknown.join(", ")}; projects: ${known.join(", ")}`,
      );
    writeHosts({ ...readHosts(), [machine]: [...rest] });
    console.log(`${machine} runs ${rest.join(", ")} (hosts.json)`);
    return hostsCommand(["sync"], all);
  }
  const hosts = readHosts();
  if (verb === "sync") {
    const wanted = hosts[machine];
    if (!wanted) {
      console.log(
        `${machine} is not in hosts.json: nothing changed. Choose its hosts with sbn hosts here <project...>.`,
      );
      return 0;
    }
    const plan = hostPlan(wanted, hostUnits(), known);
    for (const name of plan.unknown)
      console.error(`hosts.json names ${name}, which is not a project here`);
    for (const name of plan.enable) {
      const code = run("systemctl", [
        "--user",
        "enable",
        "--now",
        `signalbox@${name}`,
      ]);
      if (code !== 0) return code;
      console.log(`started and enabled signalbox@${name}`);
    }
    for (const name of plan.disable) {
      const code = run("systemctl", [
        "--user",
        "disable",
        "--now",
        `signalbox@${name}`,
      ]);
      if (code !== 0) return code;
      console.log(
        `stopped and disabled signalbox@${name} (not this machine's)`,
      );
    }
    if (!plan.enable.length && !plan.disable.length)
      console.log(`${machine} already runs exactly ${wanted.join(", ")}`);
    // The console shows every project, so it runs wherever any host does.
    const consoleUnit = "signalbox-console";
    const consoleActive =
      spawnSync("systemctl", ["--user", "is-active", consoleUnit], {
        encoding: "utf8",
      }).stdout.trim() === "active";
    if (!consoleActive) {
      const code = run("systemctl", ["--user", "enable", "--now", consoleUnit]);
      if (code !== 0) return code;
      console.log(`started and enabled ${consoleUnit}`);
    }
    return plan.unknown.length ? 1 : 0;
  }
  if (verb !== undefined)
    throw new Error(`Unknown hosts command ${verb}\n\n${HELP}`);
  const units = hostUnits();
  for (const [name, projects] of Object.entries(hosts))
    console.log(
      `${name === machine ? "* " : "  "}${name}\t${projects.join(", ")}`,
    );
  if (!hosts[machine]) console.log(`* ${machine}\t(not in hosts.json)`);
  console.log(`running here: ${units.active.join(", ") || "none"}`);
  return 0;
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
  if (command === "hosts") return hostsCommand(args, all);
  if (command === "links") return linksCommand();
  if (command === "list") {
    for (const project of all) {
      const state = spawnSync(
        "systemctl",
        ["--user", "is-active", `signalbox@${project.name}`],
        { encoding: "utf8" },
      );
      console.log(
        `${project.name}\t${state.stdout.trim()}\t${project.roots.join(", ")}\tapi :${project.apiPort}`,
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
      const { webPort, apiPort: consoleApi } = consolePorts();
      const page = `http://127.0.0.1:${webPort}/${project.name}/`;
      if (process.env.DISPLAY || process.env.WAYLAND_DISPLAY)
        return run("xdg-open", [page]);
      // No display here (an SSH session): the browser is on the other end of the connection.
      const here = process.env.SSH_CONNECTION?.split(" ")[2] ?? "this-machine";
      console.log(
        `No display here. On the computer you connect from, forward the console's ports and the host's:\n\n` +
          `  ssh -N -L ${webPort}:127.0.0.1:${webPort} -L ${consoleApi}:127.0.0.1:${consoleApi} -L ${project.apiPort}:127.0.0.1:${project.apiPort} ${userInfo().username}@${here}\n\n` +
          `then open http://localhost:${webPort}/${project.name}/ there. The forward lasts while that ssh runs.\n` +
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
    // The console on your tailnet, for a phone or another machine: Tailscale's own
    // HTTPS address, proxied to the console and to this machine's hosts, which accept
    // it (they read the tailnet's addresses too).
    case "tailnet": {
      const on = args[0] !== "off";
      const mine = readHosts()[hostname()] ?? [];
      const hosts = all.filter((candidate) => mine.includes(candidate.name));
      for (const serve of serveCommands(
        serveRoutes(consolePorts(), hosts),
        on,
      )) {
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
