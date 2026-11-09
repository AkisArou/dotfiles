// signalbox for nts: everything signalbox knows of nts is in this folder.
// signalbox itself is generic, and nts carries no signalbox file (nts never
// depends on it): its status comes from its own tools' JSON, read by the
// adapters beside this file (landing.ts, full-check.ts, machine.ts, records.ts), and
// nothing watches its files. Your global configuration gives these
// settings to nts's folder:
//
//   // ~/.config/signalbox/config.ts
//   import { nts } from "./nts/nts.ts";
//   export default defineConfig({ …, projects: { "~/Projects/nts": nts } });
//
// Then `signalbox` started anywhere under ~/Projects/nts shows nts. The
// dispatcher is off: `signalbox dispatcher --dry-run` in ~/Projects/nts shows
// what it would do. The agents themselves come from the global configuration
// (`codex()`, `claudeCode()`), which shows a project only the agents working
// under its roots.

import { brief, defineConfig, evidence, files, github, gitWorktrees, layout, machine, programs, rank, refuse, requires, review, setupReview, take, views, when, type TableRow } from "signalbox";
import { FULL_CHECK_TABLES, ntsFullCheck } from "./full-check.ts";
import { LANDING, landingActions, landingLog, ntsLanding } from "./landing.ts";
import { MACHINE_TABLES, ntsMachine } from "./machine.ts";
import { now } from "./now.tsx";
import { ntsPool } from "./pool.ts";
import { BENCHMARK_TABLES, CONFORMANCE_TABLES, ntsBenchmarks, ntsConformance } from "./records.ts";
import { tasks } from "./streams.ts";

const NTS = "~/Projects/nts";
/** Each kind of agent's instructions, one Markdown file each, kept beside this file. */
const instructions = (kind: string) => brief.file(`~/.config/signalbox/nts/agents/${kind}.md`);

/** A change handed in for review names its commit and what landing it owes. */
const landable = requires({
  commit: "the commit to land",
  owes: "the steps `node tooling/landing/check.ts <commit> --dry-run` says it runs",
});

/** A defect is reported as a gated fixture, with a control that differs in one thing and agrees with node. */
const fixture = requires({ fixture: "the path of the gated fixture", control: "the path of its control, which agrees with node" });

/** What every agent that builds shares: a tree from nts's pool, and a landable result. */
const builder = { workspace: ntsPool(), results: { review: landable } };

/** Whether a row has a cell in red: a red verdict, a culprit, a failed unit, memory below its mark. */
const red = (row: TableRow) => Object.values(row).some((cell) => typeof cell === "object" && cell.color === "red");

/** nts's own tabs: the landing check's runs alone, the full check's tables, the machine's. */
const landing = views.runs({ id: "landing", label: "Landing", sources: [LANDING], empty: "The landing check isn't read yet." });
const fullCheck = views.tables(FULL_CHECK_TABLES, { id: "full-check", label: "Full check", alert: red, empty: "The full check isn't read yet." });
const machineTab = views.tables(MACHINE_TABLES, { id: "machine", label: "Machine", alert: red, empty: "The machine isn't read yet." });
/** What nts measured: the newest benchmark run, and the records each backend is held to, each saying how old it is first. */
const benchmarks = views.tables(BENCHMARK_TABLES, { id: "benchmarks", label: "Benchmarks", empty: "The benchmarks aren't read yet." });
const conformance = views.tables(CONFORMANCE_TABLES, { id: "conformance", label: "Conformance", empty: "The conformance records aren't read yet." });
/** The plan (docs/plan.md): its outline beside the section selected, read again when the file changes. */
const plan = views.document("docs/plan.md", { id: "plan", label: "Plan" });

export const nts = defineConfig(
  // Tasks: issues labelled `task` (.github/labels.json); one held or parked is listed under its state, and no
  // agent takes it. Needs you: those waiting on you.
  github("AkisArou/nts", {
    name: "nts",
    isTask: take.labelled("task"),
    needsYou: take.labelled("state:needs-user"),
    states: { "state:held": "held", "state:parked": "parked" },
    newTaskLabels: ["task"],
    intervalSeconds: 120,
  }),
  {
    title: "nts",
    // nts's folder, and its pool's trees, which nts keeps in ~/.cache/nts-pool.
    roots: [NTS, "~/.cache/nts-pool"],
    // nts's own status, from its tools' JSON: nothing watches its files.
    providers: [gitWorktrees({ repos: [NTS] }), ntsLanding(), ntsFullCheck(), ntsMachine(), ntsBenchmarks(), ntsConformance(), files({ id: "nts-docs", label: "nts docs", paths: ["docs/plan.md"] }), machine({ disks: ["/"] })],
    // The dispatcher takes work from nts's issues and the private local board (chores); the issues keep the coordinator's state.
    boards: ["nts", "local"],
    // The coordinator's ranking first; then foundations, then wrong answers and crashes, then the rest.
    priority: rank.byCoordinator(rank.labels("kind:foundation", "kind:wrong-answer", "kind:abort")),

    agents: {
      // Coordinates: woken on others' questions, changes ready for review, main moving; writes the
      // coordinator's state, approvals, the ranking and new tasks; may start fixers and analysts.
      // Once a week it reviews the setup from what was measured, and files proposals for you to approve.
      coordinator: {
        programs: [programs.claude({ model: "claude-opus-5-5", effort: "max" })],
        instructions: instructions("coordinator"),
        always: true,
        coordinates: true,
        canStart: ["fixer", "analyst"],
        on: [
          when.every("7d").say(
            setupReview({
              canStart: "analyst",
              // What agents investigated and recorded this week.
              evidence: [evidence.files(["docs/records/**"], { limit: 40 })],
            }),
          ),
        ],
      },
      // Digs through landings, benchmarks and events for the coordinator, so its context stays on the work.
      analyst: { programs: [programs.claude({ model: "claude-sonnet-5-5" })], instructions: instructions("analyst") },
      // Foundations and performance: an agent stays in its area from task to task, and hands over at a
      // boundary it chooses once its context passes the budget.
      compiler: { ...builder, instructions: instructions("compiler"), takes: take.labelled("area:compiler"), max: 2, session: { keep: "area", budget: 400_000 } },
      runtime: { ...builder, instructions: instructions("runtime"), takes: take.labelled("area:runtime"), session: { keep: "area", budget: 400_000 } },
      // A wrong answer or a crash becomes a gated fixture with its control, then a fix.
      fixer: { ...builder, instructions: instructions("fixer"), takes: take.labelled("kind:wrong-answer", "kind:abort"), max: 2, results: { review: landable, done: fixture } },
      // Every change to compiler/ or runtime/c/ is reviewed, on a program other than its author's.
      reviewer: { instructions: instructions("reviewer"), reviews: review.touches("compiler/**", "runtime/c/**"), max: 2 },
    },

    hooks: {
      // Codex's content filter stops a session on memory-safety wording; agents say "abnormal exit".
      writing: refuse(/\bSIGSEGV\b/i, /\bsegfault/i, /\buse[- ]after[- ]free\b/i, /\bbuffer overflow\b/i, /\bexploit/i, /\bshellcode\b/i),
    },

    // The terminal's tabs: 1 to 9, 0 for the tenth, and gt/gT past it. Memory comes back once the
    // dispatcher runs agents that learn.
    screens: {
      terminal: layout.tabs([now, views.needs, views.agents, tasks, landing, fullCheck, benchmarks, conformance, plan, machineTab, views.health, views.events]),
    },
    // On a held landing: R retries it, W withdraws it; on a landing run, w shows its gate log.
    actions: [...landingActions(NTS), landingLog(NTS)],

    // Not piloted yet: `signalbox dispatcher --dry-run` in ~/Projects/nts shows what it would do.
    dispatcher: { enabled: false },
  },
);
