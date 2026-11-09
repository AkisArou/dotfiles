// nts's Now tab: one screen that answers "is nts moving, and does anything
// need me?", from what the other tabs' sources report: what waits on you, what
// is past its limits, the landing check against its ten-minute goal, the full
// check, the agents and the issues they hold, what was measured, and each
// stream's progress in plan.md's order.

import type { ReactNode } from "react";
import { defineView, gaugeState, showGauge, type FreeView, type Run, type TableCell, type Task, type ViewContext } from "signalbox";
import { Row, Spacer, Stack, Text } from "signalbox/blocks";
import { GOAL_MIN, LANDING } from "./landing.ts";
import { streamOrder } from "./streams.ts";

const cellText = (cell: TableCell | undefined) => (cell === undefined ? "" : typeof cell === "object" ? cell.text : String(cell));
const cellColor = (cell: TableCell | undefined) => (typeof cell === "object" ? cell.color : undefined);

/** A table's labelled rows, by their first column: the full check's state. */
function facts(context: ViewContext, table: string): ReadonlyMap<string, TableCell | undefined> {
  const shown = context.snapshot.tables.find((each) => each.id === table);
  return new Map((shown?.rows ?? []).map((row) => [cellText(row["what"] ?? row["part"]), row["state"] ?? row["verdict"]]));
}

function Section({ title, children }: { readonly title: string; readonly children?: ReactNode }) {
  return (
    <Stack>
      <Text bold color="cyan">
        {title}
      </Text>
      {children}
    </Stack>
  );
}

function NeedsYou({ context }: { readonly context: ViewContext }) {
  const waiting = context.snapshot.decisions;
  return (
    <Section title={`Needs you: ${waiting.length}`}>
      {waiting.length === 0 ? <Text dim>  nothing waits on you</Text> : null}
      {waiting.slice(0, 3).map((decision) => (
        <Text key={decision.id} color="yellow" wrap={false}>
          {`  ? ${decision.number ? `#${decision.number} ` : ""}${decision.title}`}
        </Text>
      ))}
      {waiting.length > 3 ? <Text dim>{`  … and ${waiting.length - 3} more on the Needs you tab`}</Text> : null}
    </Section>
  );
}

/** What is amber or red now, red first: the Health tab's gauges, in a line each. */
function Health({ context }: { readonly context: ViewContext }) {
  const past = context.snapshot.gauges.map((gauge) => ({ gauge, state: gaugeState(gauge) })).filter(({ state }) => state !== "ok");
  past.sort((a, b) => (a.state === b.state ? 0 : a.state === "red" ? -1 : 1));
  const red = past.filter(({ state }) => state === "red").length;
  return (
    <Section title={past.length ? `Health: ${red ? `${red} red` : ""}${red && past.length > red ? ", " : ""}${past.length > red ? `${past.length - red} amber` : ""}` : "Health"}>
      {past.length === 0 ? <Text dim>  everything is within its limits</Text> : null}
      {past.map(({ gauge, state }) => (
        <Text key={`${gauge.source?.id}:${gauge.id}`} color={state === "red" ? "red" : "yellow"} wrap={false}>
          {`  ${state === "red" ? "●" : "○"} ${gauge.label}: ${showGauge(gauge)}`}
          {gauge.why ? <Text dim>{`  ${gauge.why}`}</Text> : null}
        </Text>
      ))}
    </Section>
  );
}

/** The benchmarks and the records in a line each per backend, with how old they are. */
function Measured({ context }: { readonly context: ViewContext }) {
  const rows = (id: string) => context.snapshot.tables.find((table) => table.id === id)?.rows ?? [];
  const finished = rows("bench-run").find((row) => cellText(row["what"]) === "finished")?.["state"];
  const versusNode = rows("bench-counts").filter((row) => cellText(row["versus"]) === "node");
  const test262 = rows("conformance").filter((row) => cellText(row["record"]).endsWith(" test262"));
  return (
    <Section title="Measured">
      <Text>
        {"  benchmarks, against node: "}
        {versusNode.map((row) => `${cellText(row["backend"])} ${cellText(row["won"]) || 0} won, ${cellText(row["lost"]) || 0} lost`).join(" · ") || "not read yet"}
      </Text>
      {finished ? (
        <Text {...(cellColor(finished) ? { color: cellColor(finished) } : {})}>{`    from the run finished ${cellText(finished)}`}</Text>
      ) : null}
      <Text>{`  test262 passing: ${test262.map((row) => `${cellText(row["record"]).replace(" test262", "")} ${cellText(row["pass"])}`).join(" · ") || "not read yet"}`}</Text>
      {test262[0]?.["written"] ? <Text {...(cellColor(test262[0]["written"]) ? { color: cellColor(test262[0]["written"]) } : {})}>{`    recorded ${cellText(test262[0]["written"])}`}</Text> : null}
    </Section>
  );
}

function Landing({ context }: { readonly context: ViewContext }) {
  const summary = new Map(context.snapshot.summaries.find((each) => each.source.id === LANDING)?.facts ?? []);
  const runs = context.snapshot.runs.filter((run) => run.source?.id === LANDING);
  const checking: Run | undefined = runs.find((run) => run.state === "running");
  const queued = runs.filter((run) => run.state === "queued").length;
  const held = runs.filter((run) => run.state === "held").length;
  const minutes = (label: string) => Number(/([\d.]+) min/.exec(summary.get(label) ?? "")?.[1] ?? NaN);
  const median = minutes("median");
  const longest = minutes("longest");
  // The ten-minute goal: a median past eight wants a look, past ten it is missed; a change past fifteen is long.
  const medianColor = median > GOAL_MIN ? "red" : median > 0.8 * GOAL_MIN ? "yellow" : "green";
  return (
    <Section title="Landing">
      <Text>{`  ${summary.get("service") ?? "not read yet"}`}</Text>
      {checking ? <Text>{`  checking ${checking.title}: ${checking.facts?.find(([label]) => label === "elapsed")?.[1] ?? ""}`}</Text> : null}
      <Text {...(held ? { color: "yellow" as const } : {})}>{`  ${queued} waiting${held ? `, ${held} held` : ""}`}</Text>
      <Text>
        {`  today ${summary.get("today") ?? "?"} · median `}
        <Text color={medianColor}>{Number.isNaN(median) ? "?" : `${median.toFixed(1)} min`}</Text>
        {` · longest `}
        <Text color={longest > 15 ? "red" : longest > GOAL_MIN ? "yellow" : "green"}>{Number.isNaN(longest) ? "?" : `${longest.toFixed(1)} min`}</Text>
        {` (goal ${GOAL_MIN} min)`}
      </Text>
    </Section>
  );
}

function FullCheck({ context }: { readonly context: ViewContext }) {
  const state = facts(context, "full-check-state");
  const parts = facts(context, "full-check-parts");
  const culprits = context.snapshot.tables.find((each) => each.id === "full-check-culprits")?.rows.length ?? 0;
  const line = (label: string, cell: TableCell | undefined) => (
    <Text key={label}>
      {`  ${label} `}
      <Text {...(cellColor(cell) ? { color: cellColor(cell) } : {})}>{cellText(cell) || "?"}</Text>
    </Text>
  );
  return (
    <Section title="Full check">
      {line("controller", state.get("controller"))}
      {line("now", state.get("now"))}
      {line("verified behind main:", state.get("behind"))}
      <Text>
        {"  core "}
        <Text {...(cellColor(parts.get("core")) ? { color: cellColor(parts.get("core")) } : {})}>{cellText(parts.get("core")) || "?"}</Text>
        {" · wide "}
        <Text {...(cellColor(parts.get("wide")) ? { color: cellColor(parts.get("wide")) } : {})}>{cellText(parts.get("wide")) || "?"}</Text>
        {culprits ? <Text color="red">{` · ${culprits} culprit${culprits === 1 ? "" : "s"}`}</Text> : " · no culprits"}
      </Text>
      {line("count", state.get("count"))}
    </Section>
  );
}

function Agents({ context }: { readonly context: ViewContext }) {
  const held = (name: string) => context.snapshot.tasks.filter((task) => task.owner === name);
  const working = context.snapshot.agents.filter((agent) => agent.activity === "working");
  const blocked = context.snapshot.agents.filter((agent) => agent.activity === "waiting" || agent.activity === "error");
  return (
    <Section title={`Agents: ${working.length} working${blocked.length ? `, ${blocked.length} blocked` : ""}`}>
      {context.snapshot.agents.map((agent) => (
        <Row key={agent.id}>
          <Text {...(agent.activity === "working" ? {} : { dim: true })}>{`  ${agent.name}`}</Text>
          <Text dim>{`  ${agent.activity}`}</Text>
          <Text>{held(agent.name).map((task) => `  #${task.id} ${task.title}`).join("") || ""}</Text>
        </Row>
      ))}
      {context.snapshot.agents.length === 0 ? <Text dim>  none running</Text> : null}
    </Section>
  );
}

function Streams({ context }: { readonly context: ViewContext }) {
  const order = streamOrder(context);
  const streams = new Map<string, { title: string; done: number; total: number; open: number; working: number }>();
  for (const task of context.snapshot.tasks as readonly Task[]) {
    if (!task.parent) continue;
    const stream = streams.get(task.parent.id) ?? { title: task.parent.title, done: task.parent.progress?.done ?? 0, total: task.parent.progress?.total ?? 0, open: 0, working: 0 };
    stream.open += 1;
    if (task.status === "in_progress") stream.working += 1;
    streams.set(task.parent.id, stream);
  }
  const shown = [...streams].sort(([a], [b]) => (order.get(a) ?? order.size) - (order.get(b) ?? order.size));
  const widest = Math.max(0, ...shown.map(([, stream]) => stream.title.length));
  return (
    <Section title="Streams">
      {shown.map(([id, stream]) => (
        <Row key={id}>
          <Text>{`  ${stream.title.padEnd(widest)}`}</Text>
          <Text color={stream.total && stream.done === stream.total ? "green" : undefined}>{`  ${stream.done}/${stream.total} done`}</Text>
          <Text dim>{`  ${stream.open} open${stream.working ? `, ${stream.working} in progress` : ""}`}</Text>
          <Spacer />
        </Row>
      ))}
    </Section>
  );
}

/** nts at a glance. */
export const now: FreeView = defineView({
  id: "now",
  label: "Now",
  render: (context) => (
    <Stack gap={1}>
      <NeedsYou context={context} />
      <Health context={context} />
      <Landing context={context} />
      <FullCheck context={context} />
      <Agents context={context} />
      <Measured context={context} />
      <Streams context={context} />
    </Stack>
  ),
  // What waits on you, alerting; or else what is red, alerting.
  badge: (context) => {
    const red = context.snapshot.gauges.filter((gauge) => gaugeState(gauge) === "red").length;
    return context.snapshot.decisions.length ? { count: String(context.snapshot.decisions.length), alert: true } : red ? { count: String(red), alert: true } : {};
  },
});
