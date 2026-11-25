// sbn: one command for the Signalbox projects under ~/.config/signalbox.
// It finds the project from the current directory (each project's project.json
// names its roots), and runs that project's host unit, TUI and checks, and the
// console (every project's web pages and notifications, console/).
import { spawnSync } from "node:child_process";
import {
  existsSync,
  lstatSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  realpathSync,
  rmSync,
  statSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { homedir, hostname, userInfo } from "node:os";
import { basename, dirname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import {
  gitHubRepo,
  nextApiPort,
  projectName,
  projectTitle,
  scaffold,
  setupFor,
} from "./init.ts";

interface Project {
  readonly name: string;
  readonly directory: string;
  readonly roots: readonly string[];
  readonly apiPort: number;
}

const HELP = `sbn [--project <name>] [--api <ws-url>] [command]

  init [--name <n>] [--title <t>] [--check <command>] [--setup <command>] [--tasks local|github]
                    make the git repository here a project: the kit's git project in its
                    .signalbox/ folder, linked and installed. Tasks are this machine's
                    board (sbn task add) unless --tasks github; a reviewed change lands
                    as it is unless --check names a command it must pass first

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
  sdk               build the Signalbox SDK checkout's packages (~/Projects/signalbox, or
                    SIGNALBOX_SDK), then install the projects: each one's copy of the SDK
                    is the build's
  update [--no-pull]
                    after pulling, or to pull: fast-forward the dotfiles, the SDK and the
                    linked projects' repositories (local changes are kept; one whose
                    changes an incoming commit would overwrite, or with commits of its
                    own, is left as it is), sbn sdk, reload the units, and restart this
                    machine's hosts and the console
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

/** The projects here: each folder, or link to a folder elsewhere (links.json), with a project.json. */
export function projects(directory: string = home): Project[] {
  const isProject = (name: string) => {
    try {
      return (
        statSync(join(directory, name)).isDirectory() &&
        existsSync(join(directory, name, "project.json"))
      );
    } catch {
      return false;
    }
  };
  return readdirSync(directory)
    .filter(isProject)
    .map((name) => {
      const manifest = JSON.parse(
        readFileSync(join(directory, name, "project.json"), "utf8"),
      ) as Omit<Project, "directory">;
      return {
        ...manifest,
        directory: join(directory, name),
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
/**
 * The apps workspace's package list with the linked projects in it. pnpm does not
 * install a project through a folder link, and would count it twice beside its
 * own folder, so each link is left out of "*" and its folder is named, relative
 * to the workspace (the same on every machine with the same layout).
 */
export function workspaceWithLinks(
  yaml: string,
  links: Links,
  workspace: string,
): string {
  const section = /^packages:\n(?:  .*\n)*/m;
  if (!section.test(yaml))
    throw new Error("pnpm-workspace.yaml has no packages list");
  const linked = Object.entries(links).sort(([a], [b]) => a.localeCompare(b));
  const lines = [
    "packages:",
    '  - "*"',
    ...(linked.length
      ? [
          "  # Kept by sbn links from links.json: each linked project installs from its own folder.",
          ...linked.flatMap(([name, folder]) => [
            `  - "!${name}"`,
            `  - "${relative(workspace, expand(folder))}"`,
          ]),
        ]
      : []),
  ];
  return yaml.replace(section, `${lines.join("\n")}\n`);
}
/**
 * Builds the SDK checkout's packages, then installs the projects' workspace, which copies
 * each project's SDK packages from that build. pnpm runs in those two folders, so it
 * takes their Node and pnpm: never a project's own repository's (nable-solutions pins
 * others).
 */
function sdkCommand(): number {
  const sdk = expand(process.env.SIGNALBOX_SDK ?? "~/Projects/signalbox");
  const steps: [string, readonly string[], string][] = [
    ["the SDK's packages install", ["install"], sdk],
    ["the SDK builds", ["-r", "--if-present", "run", "build"], sdk],
    // From the store when it has a package; a package this machine never fetched (a new
    // SDK dependency) is fetched, with the metadata pnpm's release-age policy checks.
    ["the projects install", ["install", "--prefer-offline"], home],
  ];
  for (const [what, args, cwd] of steps) {
    const result = spawnSync("pnpm", args, { cwd, stdio: "inherit" });
    if (result.error || result.status !== 0) {
      console.error(
        `sbn sdk: ${what} failed (pnpm ${args.join(" ")} in ${cwd})`,
      );
      return 1;
    }
  }
  return 0;
}

/** The repositories sbn update pulls: each folder's repository, once (a missing folder has none). */
export function updateRepositories(
  folders: readonly string[],
  toplevel: (folder: string) => string | undefined,
): string[] {
  return [
    ...new Set(
      folders.flatMap((folder) => {
        const top = toplevel(folder);
        return top ? [top] : [];
      }),
    ),
  ];
}

/**
 * Brings this machine's Signalbox to its checkouts: pulls them (fast-forward only), builds
 * the SDK and installs the projects, reloads the units, and restarts what runs here. A
 * host or console that is stopped stays stopped.
 */
function updateCommand(args: readonly string[]): number {
  const sdk = expand(process.env.SIGNALBOX_SDK ?? "~/Projects/signalbox");
  const git = (cwd: string, gitArgs: readonly string[]) =>
    spawnSync("git", ["-C", cwd, ...gitArgs], { encoding: "utf8" });
  if (!args.includes("--no-pull")) {
    const linksPath = join(home, "links.json");
    const links = existsSync(linksPath)
      ? parseLinks(readFileSync(linksPath, "utf8"))
      : {};
    const repositories = updateRepositories(
      [home, sdk, ...Object.values(links).map(expand)],
      (folder) =>
        existsSync(folder)
          ? git(folder, ["rev-parse", "--show-toplevel"]).stdout.trim() ||
            undefined
          : undefined,
    );
    // sbn itself is in one of them: when a pull changes it, the rest of the update is
    // the new sbn's, so a fix to the update reaches this very run.
    // Its real path: ~/.config/signalbox is a link into the dotfiles, outside which git
    // knows no file.
    const self = realpathSync(fileURLToPath(import.meta.url));
    const revisionOf = () => git(dirname(self), ["log", "-1", "--format=%H", "--", self]).stdout.trim();
    const before = revisionOf();
    // A fast-forward keeps local changes: git refuses it, and changes nothing, only
    // when an incoming change would overwrite one, and that refusal is said.
    for (const repository of repositories) {
      const local = git(repository, ["status", "--porcelain"]).stdout.trim() !== "";
      const pulled = git(repository, ["pull", "--ff-only", "--quiet"]);
      console.log(
        pulled.status === 0
          ? `${repository}: pulled${local ? " (its local changes kept)" : ""}`
          : `${repository}: not pulled (${(pulled.stderr || pulled.stdout).trim().split("\n")[0] || "no fast-forward"})`,
      );
    }
    if (revisionOf() !== before) {
      console.log("sbn changed: the rest of the update runs on the new sbn");
      const rest = spawnSync(
        process.execPath,
        [...process.execArgv, self, "update", "--no-pull", ...args.filter((arg) => arg !== "--no-pull")],
        { stdio: "inherit" },
      );
      return rest.status ?? 1;
    }
  }
  const built = sdkCommand();
  if (built !== 0) return built;
  run("systemctl", ["--user", "daemon-reload"]);
  const units = [
    ...(readHosts()[hostname()] ?? []).map((name) => `signalbox@${name}`),
    "signalbox-console",
  ];
  const restarted = run("systemctl", ["--user", "try-restart", ...units]);
  console.log(`restarted what runs here, of: ${units.join(", ")}`);
  return restarted;
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
  const workspacePath = join(home, "pnpm-workspace.yaml");
  const listed = readFileSync(workspacePath, "utf8");
  const withLinks = workspaceWithLinks(listed, links, home);
  if (withLinks !== listed) {
    writeFileSync(workspacePath, withLinks);
    console.log("pnpm-workspace.yaml lists the linked projects' folders");
  }
  if (withLinks !== listed || plan.make.length || plan.repoint.length) {
    const installed = spawnSync("pnpm", ["install"], {
      cwd: home,
      stdio: "inherit",
    });
    if (installed.status !== 0) {
      console.error("pnpm install failed; run it in ~/.config/signalbox");
      return 1;
    }
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

/** The units sbn runs, kept with it in the dotfiles (../systemd/user). */
const UNITS = ["signalbox@.service", "signalbox-console.service"];
/** The user's unit folder: where systemd finds them. */
const unitFolder = () =>
  join(
    process.env.XDG_CONFIG_HOME ?? join(homedir(), ".config"),
    "systemd/user",
  );

/**
 * Makes the units sbn starts known to systemd: one not in the user's unit folder yet
 * (a machine whose dotfiles were deployed before it existed) is linked there, as the
 * dotfiles' deploy would, and the manager reloaded.
 */
function installUnits(): number {
  const missing = UNITS.filter(
    (unit) =>
      !existsSync(join(unitFolder(), unit)) &&
      existsSync(join(home, "../systemd/user", unit)),
  );
  for (const unit of missing) {
    const code = run("systemctl", [
      "--user",
      "link",
      join(home, "../systemd/user", unit),
    ]);
    if (code !== 0) return code;
  }
  return missing.length ? run("systemctl", ["--user", "daemon-reload"]) : 0;
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
    const installed = installUnits();
    if (installed !== 0) return installed;
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
    // The console shows every project, so it runs wherever any host does: like a
    // host, running now and enabled, or it is gone after the next boot.
    const consoleUnit = "signalbox-console";
    const consoleIs = (query: "is-active" | "is-enabled") =>
      spawnSync("systemctl", ["--user", query, consoleUnit], {
        encoding: "utf8",
      }).stdout.trim();
    if (
      consoleIs("is-active") !== "active" ||
      consoleIs("is-enabled") !== "enabled"
    ) {
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

/** Reads one git value in `cwd`, or undefined when git has none. */
function gitValue(cwd: string, args: readonly string[]): string | undefined {
  const result = spawnSync("git", ["-C", cwd, ...args], { encoding: "utf8" });
  return result.status === 0 ? result.stdout.trim() || undefined : undefined;
}

/**
 * Makes the git repository around the current directory a Signalbox project: the
 * kit's git project in its .signalbox/ folder, linked here (links.json) and
 * installed. Its tasks are this machine's board unless --tasks github; a change
 * lands as reviewed unless --check names what it must pass.
 */
function initCommand(args: string[], all: readonly Project[]): number {
  const option = (name: string) => {
    const index = args.indexOf(name);
    if (index < 0) return undefined;
    const [, value] = args.splice(index, 2);
    if (!value) throw new Error(`${name} needs a value`);
    return value;
  };
  const named = option("--name"),
    title = option("--title"),
    check = option("--check"),
    setup = option("--setup"),
    tasks = option("--tasks") ?? "local";
  if (args.length) throw new Error(`sbn init takes no ${args.join(" ")}\n\n${HELP}`);
  if (tasks !== "local" && tasks !== "github")
    throw new Error("--tasks is local (this machine's board) or github (the repository's issues)");
  const top = gitValue(process.cwd(), ["rev-parse", "--show-toplevel"]);
  if (!top) throw new Error("sbn init runs inside a git repository");
  const already = projectFor(all, top);
  if (already) throw new Error(`${top} is already ${already.name}'s`);
  const name = named ?? projectName(basename(top));
  if (!/^[a-z0-9][a-z0-9-]*$/.test(name))
    throw new Error(`${name} is not a project name (lower case, letters, digits and -): use --name`);
  if (all.some((project) => project.name === name) || existsSync(join(home, name)))
    throw new Error(`There is a project ${name} already: use --name`);
  const folder = join(top, ".signalbox");
  if (existsSync(folder)) throw new Error(`${folder} is there already`);
  const remote = "origin";
  const url = gitValue(top, ["remote", "get-url", remote]);
  if (!url) throw new Error(`${top} has no remote ${remote}: its changes land by being pushed there`);
  const branch =
    gitValue(top, ["symbolic-ref", "--short", `refs/remotes/${remote}/HEAD`])?.replace(
      `${remote}/`,
      "",
    ) ??
    gitValue(top, ["branch", "--show-current"]) ??
    "main";
  const github = tasks === "github" ? gitHubRepo(url) : undefined;
  if (tasks === "github" && !github)
    throw new Error(`--tasks github needs a GitHub remote; ${remote} is ${url}`);
  const tilde = (path: string) =>
    path === homedir() || path.startsWith(homedir() + sep)
      ? `~${path.slice(homedir().length)}`
      : path;
  const installs = setup ?? setupFor(readdirSync(top));
  const files = scaffold({
    name,
    title: title ?? projectTitle(name),
    root: tilde(top),
    trees: `~/.cache/${name}-trees`,
    apiPort: nextApiPort(all.map((project) => project.apiPort)),
    remote,
    branch,
    tasks: github ? { github } : "local",
    ...(installs ? { setup: installs } : {}),
    ...(check ? { check } : {}),
    sdk: expand(process.env.SIGNALBOX_SDK ?? "~/Projects/signalbox"),
  });
  for (const [path, text] of Object.entries(files)) {
    mkdirSync(dirname(join(folder, path)), { recursive: true });
    writeFileSync(join(folder, path), text);
  }
  console.log(`wrote ${tilde(folder)}: ${Object.keys(files).length} files (commit them with ${name})`);
  const linksPath = join(home, "links.json");
  const links = existsSync(linksPath) ? parseLinks(readFileSync(linksPath, "utf8")) : {};
  writeFileSync(
    linksPath,
    `${JSON.stringify(
      Object.fromEntries(
        Object.entries({ ...links, [name]: tilde(folder) }).sort(([a], [b]) => a.localeCompare(b)),
      ),
      null,
      2,
    )}\n`,
  );
  console.log(`links.json links ${name} to ${tilde(folder)}`);
  const linked = linksCommand();
  if (linked !== 0) return linked;
  // The console bundles each project's pages as it starts.
  spawnSync("systemctl", ["--user", "try-restart", "signalbox-console"]);
  const mine = readHosts()[hostname()] ?? [];
  console.log(
    [
      "",
      `${name} is a Signalbox project. Next:`,
      `  sbn doctor --project ${name}: what this machine still lacks`,
      `  sbn hosts here ${[...mine, name].join(" ")}: run its host here (then commit hosts.json)`,
      github
        ? `  its tasks: the open issues of ${github}`
        : "  sbn task add <title...> [--body <text>]: a task on its board",
      "  sbn assign <n>: a worker takes task n",
      "  sbn web: the console",
      ...(check
        ? []
        : [
            "",
            'No --check: a reviewed change lands as it is. Set "check" in project.json to make it pass a command first.',
          ]),
    ].join("\n"),
  );
  return 0;
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
  if (command === "init") return initCommand(args, all);
  if (command === "hosts") return hostsCommand(args, all);
  if (command === "links") return linksCommand();
  if (command === "sdk") return sdkCommand();
  if (command === "update") return updateCommand(args);
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
      const routes = serveRoutes(consolePorts(), hosts);
      // The first call shows tailscale's own output: a tailnet without serve yet is
      // asked to enable it there. The others are quiet unless they fail.
      for (const [index, serve] of serveCommands(routes, on).entries()) {
        if (index === 0) {
          const code = run("tailscale", serve);
          if (code !== 0) return code;
          continue;
        }
        const result = spawnSync("tailscale", serve, { encoding: "utf8" });
        if (result.status !== 0) {
          console.error(`${result.stdout ?? ""}${result.stderr ?? ""}`.trim());
          return result.status ?? 1;
        }
      }
      if (!on) return 0;
      console.log(
        `\nServing:\n${routes.map((route) => `  ${route.path.padEnd(22)} ${route.target}`).join("\n")}`,
      );
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
    case "doctor": {
      if (!existsSync(join(project.directory, "scripts/doctor.ts"))) {
        console.error(
          `${project.name} has no scripts/doctor.ts here (is its checkout current? sbn update)`,
        );
        return 1;
      }
      return run(process.execPath, ["scripts/doctor.ts"], {
        cwd: project.directory,
      });
    }
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
