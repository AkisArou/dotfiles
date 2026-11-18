/** @jsxImportSource @opentui/react */
import { useEffect, useRef, useState } from "react";
import { useClient } from "signalbox/client/react";
import { earlierTasks, runTaskAction, taskActions } from "@signalbox/kit/shared/actions";
import type { ActionValues } from "@signalbox/kit/shared/actions";
import { written } from "./compose.ts";
import { agentStates, issueStates, taskConditions, taskPhases } from "@signalbox/kit/shared/glossary";
import * as refs from "../shared/model.ts";
import { useKeyboard, useTerminalDimensions } from "@opentui/react";
import { TextAttributes } from "@opentui/core";
import type { ScrollBoxRenderable } from "@opentui/core";
import { useProject } from "../shared/use-project.ts";
import {
  agentLines,
  agentTone,
  assignmentTone,
  issueStateTerm,
  liveTask,
  needsCount,
  settingsLine,
  tabs,
  taskWord,
} from "../shared/model.ts";
import type { Agent, Tab } from "../shared/model.ts";
import { terminalConfig } from "./config.ts";
import {
  chosenOf,
  describeFilters,
  describeTaskFilter,
  filterKey,
  filterOptions,
  matches,
  taskFacets,
} from "@signalbox/kit/shared/filters";
import type { Chosen } from "@signalbox/kit/shared/filters";
import type { TerminalTools } from "./tools.ts";
import { Line, ListDetail, ListRow, ScrollPane, Section } from "./components.tsx";
import {
  agentColor,
  AgentDetail,
  NowView,
  stateColor,
  stateMark,
  TaskDetail,
  TaskPopup,
  toneColor,
} from "./views.tsx";
import { colors as c } from "./theme.ts";
import { useSelection } from "./selection.tsx";
import { isNtsPage, tableRows, cellText } from "../status/model.ts";
import { NtsContent } from "./nts.tsx";
import { SettingDetail, settingAction, settingRows } from "./settings.tsx";
import { NeedsDetail, answerNeed, needsItems, needsLabel, withdrawNeed } from "./needs.tsx";
import type { SettingRow } from "./settings.tsx";
import { tabLabels } from "./navigation.ts";
import { navigationKey, popupKey } from "./keyboard.ts";
import { NativeTerminal } from "./terminal.tsx";

export function TerminalApp({ onExit, tools }: { onExit(): void; tools: TerminalTools }) {
  const client = useClient();
  const selection = useSelection(),
    project = useProject(),
    { width, height } = useTerminalDimensions();
  const [tab, setTab] = useState<Tab>("Now"),
    [focus, setFocus] = useState<"list" | "detail">("list");
  const [selected, setSelected] = useState<Partial<Record<Tab, string>>>({});
  const [chosen, setChosen] = useState<Chosen>(new Map());
  const [message, setMessage] = useState<string>(),
    [tool, setTool] = useState<string>();
  // The task an agent works on, shown over everything (t on an agent; Escape or t closes).
  // Over everything: an agent's task (t), or the ended sessions (e).
  const [popup, setPopup] = useState<{ kind: "task"; issue: number } | { kind: "ended" } | null>(
    null,
  );
  const popupScroll = useRef<ScrollBoxRenderable>(null);
  const [help, setHelp] = useState(false),
    [terminalRequest, setTerminalRequest] = useState(0);
  const nativeInput = useRef(false),
    borrowing = useRef(false),
    afterG = useRef(false);
  const listScroll = useRef<ScrollBoxRenderable>(null),
    detail = useRef<ScrollBoxRenderable>(null);
  const { overview } = project.data;
  // Ended sessions stay in the host's record; the list hides them until `e` shows them.
  // The list holds live sessions; the ended ones open in a popup (e).
  const endedAgents = project.data.agents.filter((item) => item.state === "ended");
  const agents = project.data.agents.filter((item) => item.state !== "ended");
  // The Agents tab ends with the settings its launches follow.
  const settings = settingRows(project.data.settings);
  const readings = {
    Overview: project.overview,
    Tasks: project.issues,
    Agents: project.agents,
    Plan: project.plan,
    ...project.nts.readings,
  };
  const failing = Object.entries(readings).flatMap(([name, reading]) =>
    reading.status === "error" ? [`${name}: ${reading.error.message}`] : [],
  );
  const taskTab = tab === "Tasks";
  const tasks = project.data.issues.filter((issue) => matches(taskFacets, chosen, issue));
  const nts = tab === "Health" || isNtsPage(tab);
  const report =
    tab === "Health" ? project.nts.health : isNtsPage(tab) ? project.nts.page(tab) : undefined;
  const evidence = tableRows(report);
  const needs = needsItems(project.data.needs);
  const items = taskTab
    ? tasks
    : tab === "Agents"
      ? [...agents, ...settings]
      : tab === "Needs you"
        ? needs
        : evidence;
  const currentId = items.find((item) => item.id === selected[tab])?.id ?? items[0]?.id;
  const task = tasks.find((item) => item.id === currentId),
    agent = agents.find((item) => item.id === currentId),
    setting = tab === "Agents" ? settings.find((item) => item.id === currentId) : undefined,
    need = tab === "Needs you" ? needs.find((item) => item.id === currentId) : undefined;
  const split = taskTab || tab === "Agents" || tab === "Needs you";
  function choose(id: string) {
    setSelected((old) => ({ ...old, [tab]: id }));
    setFocus("list");
  }
  function switchTab(next: Tab) {
    afterG.current = false;
    setTab(next);
    setFocus("list");
    setHelp(false);
    setMessage(undefined);
  }
  async function local(title: string, action: () => Promise<void>) {
    if (borrowing.current) return;
    borrowing.current = true;
    setTool(title);
    setMessage(undefined);
    try {
      await action();
    } catch (error) {
      setMessage(error instanceof Error ? error.message : String(error));
    } finally {
      borrowing.current = false;
      setTool(undefined);
    }
  }
  function change(row: SettingRow) {
    const action = settingAction(row, project, terminalConfig.editor);
    if (!action) return;
    void local(row.label, async () => {
      if (action.kind === "run") return tools.run(action.run, action.cwd);
      if (action.kind === "many") {
        const indices = await tools.findMany(action.lines, row.label, action.chosen);
        if (indices !== undefined) await action.apply(indices);
        return;
      }
      const index = await tools.find(action.lines, row.label);
      if (index !== undefined) await action.apply(index);
    });
  }
  function find() {
    const target = nts || split ? tab : "Tasks";
    const candidates = nts
      ? evidence.map((item) => ({
          id: item.id,
          line: `${item.table.title} · ${item.table.columns.map((column) => cellText(item.row[column.key])).join(" · ")}`,
        }))
      : target === "Agents"
        ? agents.map((item) => ({
            id: item.id,
            line: `${item.name} · ${item.state} · ${item.path ?? ""}`,
          }))
        : target === "Needs you"
          ? needs.map((item) => ({ id: item.id, line: needsLabel(item).label }))
          : tasks.map((item) => ({
              id: item.id,
              line: `#${item.number} ${item.title} · ${item.state}`,
            }));
    if (!candidates.length) return setMessage("No items in this view.");
    void local(`Find ${target}`, async () => {
      const index = await tools.find(
        candidates.map((item) => item.line),
        `Find ${target}`,
      );
      const found = index === undefined ? undefined : candidates[index];
      if (found) {
        switchTab(target);
        setSelected((old) => ({ ...old, [target]: found.id }));
      }
    });
  }
  // Several at once: the current filters start ticked, and the ticked set is the new filter.
  function filter() {
    const options = filterOptions(
      taskFacets,
      chosen,
      project.data.issues,
      describeTaskFilter(project.data.labels),
    );
    const lines = [
      "✕ clear the filters",
      ...options.map(
        (option) =>
          `${option.label}  (${option.count})${option.meaning ? `  — ${option.meaning}` : ""}`,
      ),
    ];
    const ticked = options.flatMap((option, index) => (option.selected ? [index + 1] : []));
    void local("Filter Tasks", async () => {
      const picked = await tools.findMany(lines, "Filter Tasks", ticked);
      if (picked === undefined) return;
      setChosen(
        picked.includes(0)
          ? new Map()
          : chosenOf(
              picked.flatMap((index) => {
                const option = options[index - 1];
                return option ? [filterKey(option.facet, option.value)] : [];
              }),
            ),
      );
      setFocus("list");
    });
  }
  // What you can do to the selected task (@signalbox/kit/shared/actions): an action whose note is
  // optional is offered plain and "with a note…"; words open the editor; urgency is a pick.
  function act() {
    if (!task) return setMessage("Select a task first.");
    const issue = task;
    const live = liveTask(project.data.assignments, issue.number);
    const actions = taskActions(issue, live, project.data.settings?.next);
    if (!actions.length) return setMessage(`#${issue.number} is not a task: nothing to do here.`);
    const choices = actions.flatMap((action) =>
      action.input === "optional"
        ? [
            { action, note: false, line: `${action.label}  — ${action.meaning}` },
            { action, note: true, line: `${action.label}, with a note…` },
          ]
        : [
            {
              action,
              note: action.input === "required",
              line: `${action.label}  — ${action.meaning}`,
            },
          ],
    );
    void local(`#${issue.number}`, async () => {
      const picked = await tools.find(
        choices.map((choice) => choice.line),
        `#${issue.number} ${live ? taskWord(live) : issueStateTerm(issue.state).label}`,
      );
      const choice = picked === undefined ? undefined : choices[picked];
      if (!choice) return;
      const { action } = choice;
      const values: ActionValues = {};
      if (action.input === "urgency") {
        const levels = Array.from({ length: 19 }, (_, index) => 9 - index);
        const at = await tools.find(
          levels.map(
            (level) => `${level > 0 ? "+" : ""}${level}${level === 0 ? "  the default" : ""}`,
          ),
          `#${issue.number} urgency (higher first)`,
        );
        if (at === undefined) return;
        values.urgency = levels[at]!;
      } else if (choice.note) {
        const text = await written(tools, terminalConfig.editor, `${action.id}-${issue.number}`, [
          `#${issue.number} ${issue.title}`,
          `${action.label}: ${action.meaning}`,
        ]);
        if (text === undefined) return setMessage(`${action.label}: cancelled, nothing written.`);
        values.text = text;
      }
      const worker = project.data.settings?.roles.worker;
      if (action.id === "assign" && worker?.pick === "coordinator" && worker.models.length > 1) {
        const at = await tools.find(
          ["The first available model", ...worker.models.map((model) => model.label)],
          `#${issue.number} worker model`,
        );
        if (at === undefined) return;
        if (at > 0) values.model = worker.models[at - 1]!.id;
      }
      const done = await project.perform(() =>
        runTaskAction(project.commands, action.id, issue.number, live, values),
      );
      if (done !== undefined) setMessage(`#${issue.number}: ${action.label.toLowerCase()}, done.`);
    });
  }
  function newTerminal() {
    if (!project.online || project.busy) return;
    const programs = project.terminalPrograms;
    if (!programs.length) return setMessage("Terminal programs are not available yet.");
    void local("New terminal", async () => {
      const picked = await tools.find(
        programs.map((program) => program.label),
        "New terminal",
      );
      const program = picked === undefined ? undefined : programs[picked];
      if (!program) return;
      const result = await project.perform(() =>
        project.commands.startTerminal.run({ program: program.id }),
      );
      if (result) {
        setSelected((old) => ({ ...old, Agents: result.id }));
        setFocus("list");
      }
    });
  }
  function git(main = false) {
    const workspace = main
      ? overview?.root
      : tab === "Agents"
        ? (agent?.path ?? overview?.root)
        : (task?.workspace ??
          agents.find((item) => item.issue === task?.number)?.path ??
          overview?.root);
    if (!workspace) return setMessage("Project directory is not available yet.");
    void local(`lazygit · ${workspace}`, () => tools.run(terminalConfig.git, workspace));
  }
  function move(step: number) {
    if (!items.length) return;
    setSelected((old) => {
      const index = Math.max(
        0,
        items.findIndex((item) => item.id === (old[tab] ?? currentId)),
      );
      return { ...old, [tab]: items[Math.max(0, Math.min(items.length - 1, index + step))]!.id };
    });
    setFocus("list");
  }
  useKeyboard((key) => {
    if (nativeInput.current) {
      if (key.ctrl && key.name === "]") {
        key.preventDefault();
        key.stopPropagation();
        nativeInput.current = false;
        setFocus("list");
      }
      return;
    }
    if (popup !== null) {
      key.preventDefault();
      const opener = popup.kind === "task" ? "t" : "e";
      const { action, afterG: prefix } = popupKey(key, afterG.current, opener);
      afterG.current = prefix;
      const pane = popupScroll.current;
      const page = Math.max(1, (pane?.viewport.height ?? 10) - 1);
      if (action.kind === "close") setPopup(null);
      else if (action.kind === "scroll")
        pane?.scrollBy(
          action.direction *
            (action.distance === "line"
              ? 1
              : action.distance === "half"
                ? Math.max(1, Math.floor(page / 2))
                : page),
        );
      else if (action.kind === "edge") pane?.scrollTo(action.last ? Infinity : 0);
      return;
    }
    if (tab === "Agents" && agent?.terminal) key.preventDefault();
    if (key.ctrl && (key.name === "q" || key.name === "c")) {
      key.preventDefault();
      onExit();
      return;
    }
    if (borrowing.current) {
      afterG.current = false;
      key.preventDefault();
      return;
    }
    const plain = !key.ctrl && !key.meta && !key.option;
    const navigation = navigationKey(key, afterG.current);
    afterG.current = navigation.afterG;
    if (key.name === "escape") {
      setFocus("list");
      setHelp(false);
      setMessage(undefined);
      project.dismissError();
      return;
    }
    if (key.name === "q" && plain) {
      onExit();
      return;
    }
    const number = key.name === "0" ? 9 : Number(key.name) - 1;
    if (plain && number >= 0 && number < Math.min(10, tabs.length)) {
      switchTab(tabs[number]!);
      return;
    }
    if (plain && (key.name === "?" || key.sequence === "?")) {
      setHelp((old) => !old);
      return;
    }
    const motion = navigation.motion;
    if (motion) {
      key.preventDefault();
      if (motion.kind === "prefix") return;
      if (motion.kind === "tab") {
        switchTab(tabs[(tabs.indexOf(tab) + tabs.length + motion.direction) % tabs.length]!);
        return;
      }
      const inList = !help && (split || nts) && focus === "list";
      const pane = inList && !nts ? listScroll.current : detail.current;
      if (motion.kind === "edge") {
        if (inList) move(motion.last ? items.length : -items.length);
        else pane?.scrollTo(motion.last ? Infinity : 0);
        return;
      }
      const viewport = Math.max(1, pane?.viewport.height ?? height - 8);
      const lines =
        motion.distance === "line"
          ? 1
          : motion.distance === "half"
            ? Math.max(1, Math.floor(viewport / 2))
            : viewport;
      const row = currentId ? pane?.findDescendantById(`row-${currentId}`) : undefined;
      const step =
        inList && motion.distance !== "line"
          ? Math.max(1, Math.round(lines / Math.max(1, row?.height ?? 1)))
          : lines;
      if (inList) move(motion.direction * step);
      else pane?.scrollBy(motion.direction * lines);
      return;
    }
    if (help || !plain) return;
    if (key.name === "n" && tab === "Agents") {
      newTerminal();
      return;
    }
    if (key.name === "t" && tab === "Agents") {
      if (agent?.issue) setPopup({ kind: "task", issue: agent.issue });
      else setMessage(agent ? `${agent.name} works on no task.` : "Select an agent first.");
      return;
    }
    if (key.name === "e" && tab === "Agents") {
      if (endedAgents.length) setPopup({ kind: "ended" });
      else setMessage("No session has ended.");
      return;
    }
    if (key.name === "/") {
      find();
      return;
    }
    if (key.name === "f" && !key.shift && taskTab) {
      filter();
      return;
    }
    if (key.name === "a" && !key.shift && taskTab) {
      act();
      return;
    }
    if ((key.name === "F" || (key.name === "f" && key.shift)) && taskTab) {
      setChosen(new Map());
      return;
    }
    if (key.name === "L" || (key.name === "l" && key.shift)) {
      git(true);
      return;
    }
    if (key.name === "l") {
      git();
      return;
    }
    if (need && key.name === "x") {
      void local("Needs you", async () => {
        const said = await withdrawNeed(need, project, tools);
        if (said) setMessage(said);
      });
      return;
    }
    if (need && key.name === "return") {
      key.preventDefault();
      void local("Needs you", async () => {
        const said = await answerNeed(need, project, tools, terminalConfig.editor);
        if (said) setMessage(said);
      });
      return;
    }
    if (setting && key.name === "return") {
      key.preventDefault();
      change(setting);
      return;
    }
    if ((split || nts) && (key.name === "return" || key.name === "right")) {
      key.preventDefault();
      if (tab === "Agents" && agent?.terminal) setTerminalRequest((value) => value + 1);
      setFocus("detail");
      return;
    }
    if ((split || nts) && (key.name === "left" || key.name === "h")) {
      key.preventDefault();
      setFocus("list");
    }
  });
  const label = describeFilters(taskFacets, chosen);
  const count = (name: Tab) =>
    name === "Tasks"
      ? tasks.length
      : name === "Agents"
        ? agents.length
        : name === "Needs you"
          ? needsCount(project.data.needs)
          : undefined;
  // A task's place: its group, else its stream; a group's place is its stream.
  const groupOf = (issue: (typeof tasks)[number]) =>
    issue.labels.includes("kind:group") ? issue.stream : issue.group || issue.stream;
  // An agent's lines under its name: the issue it works on, then its role and tree.
  const agentDetails = (agent: Agent) => agentLines(agent, project.data.issues, shortPath);
  const plural = (count: number, noun: string) => `${count} ${noun}${count === 1 ? "" : "s"}`;
  const home = process.env.HOME ?? "";
  const shortPath = (path: string) =>
    home && path.startsWith(home) ? `~${path.slice(home.length)}` : path;
  const scroll = (
    <ScrollPane
      key={`${tab}-list`}
      scrollRef={listScroll}
      focused={focus === "list" && !help}
      selected={currentId ? `row-${currentId}` : undefined}
      onFocus={() => setFocus("list")}
    >
      <Line color={c.muted}>
        {taskTab
          ? `${tasks.length} of ${project.data.issues.length}${label ? ` · ${label}` : ""}`
          : tab === "Needs you"
            ? `${project.data.needs.questions.length} questions · ${project.data.needs.unasked.length} not asked yet · Enter answers`
            : `${plural(agents.length, "agent")}${endedAgents.length ? ` · ${endedAgents.length} ended (e)` : ""}`}
      </Line>
      {tab === "Needs you" &&
        needs.map((item) => {
          const { label: text, meta, end } = needsLabel(item);
          return (
            <ListRow
              key={item.id}
              id={`row-${item.id}`}
              selected={item.id === currentId}
              focused={focus === "list"}
              label={`${item.kind === "answered" ? "✓" : "?"} ${text}`}
              {...(meta ? { meta } : {})}
              end={end}
              color={item.kind === "answered" ? c.muted : c.warning}
              onSelect={() => choose(item.id)}
            />
          );
        })}
      {taskTab &&
        tasks.map((issue) => {
          // A task the coordinator queued says where it is; any other issue, its labels' state.
          const live = liveTask(project.data.assignments, issue.number);
          return (
            <ListRow
              key={issue.id}
              id={`row-${issue.id}`}
              selected={issue.id === currentId}
              focused={focus === "list"}
              label={`${stateMark(issue.state)} #${issue.number} ${issue.title}`}
              meta={groupOf(issue)}
              end={
                live
                  ? taskWord(live)
                  : (issue.claims?.[0]?.what ?? issueStateTerm(issue.state).label)
              }
              color={live ? toneColor(assignmentTone(live)) : stateColor(issue.state)}
              onSelect={() => choose(issue.id)}
            />
          );
        })}
      {tab === "Agents" &&
        agents.map((item) => (
          <ListRow
            key={item.id}
            id={`row-${item.id}`}
            selected={item.id === currentId}
            focused={focus === "list"}
            label={`● ${item.name}`}
            details={agentDetails(item)}
            end={item.state}
            color={agentColor(item.state)}
            onSelect={() => choose(item.id)}
          />
        ))}
      {!items.length && (
        <Line color={c.muted}>
          {taskTab
            ? "No matching tasks. f changes filters; F clears them."
            : tab === "Needs you"
              ? "Nothing waits on you."
              : "No native terminals. n opens a session."}
        </Line>
      )}
    </ScrollPane>
  );
  // The Agents tab's settings stay at the bottom of its list, however many agents scroll above.
  const list =
    tab === "Agents" && settings.length > 0 ? (
      <box flexDirection="column" flexGrow={1} minHeight={0}>
        {scroll}
        <box flexDirection="column" flexShrink={0} border={["top"]} borderColor={c.border}>
          <Line color={c.muted}>{" settings · Enter changes"}</Line>
          {settings.map((row) => (
            <ListRow
              key={row.id}
              id={`row-${row.id}`}
              selected={row.id === currentId}
              focused={focus === "list"}
              label={row.label}
              end={row.value}
              color={row.warn ? c.warning : c.text}
              onSelect={() => choose(row.id)}
            />
          ))}
        </box>
      </box>
    ) : (
      scroll
    );
  const content =
    tab === "Agents" && agent?.terminal ? (
      <NativeTerminal
        key={agent.terminal}
        id={agent.terminal}
        interactive={focus === "detail"}
        controlRequest={terminalRequest}
        onControl={(owned) => {
          nativeInput.current = owned;
        }}
      />
    ) : (
      <ScrollPane
        key={`${tab}-detail-${nts ? focus : ""}`}
        scrollRef={detail}
        selected={nts && focus === "list" && currentId ? `row-${currentId}` : undefined}
        focused={(!split || focus === "detail") && !help}
        onFocus={() => split && setFocus("detail")}
      >
        {nts && (
          <NtsContent
            project={project}
            page={tab}
            width={width - 4}
            selected={currentId}
            details={focus === "detail"}
            onSelect={choose}
          />
        )}
        {tab === "Now" && <NowView project={project} />}
        {taskTab && task && (
          <TaskDetail
            issue={task}
            task={liveTask(project.data.assignments, task.number)}
            earlier={earlierTasks(project.data.assignments, task.number)}
            actions={taskActions(
              task,
              liveTask(project.data.assignments, task.number),
              project.data.settings?.next,
            )}
          />
        )}
        {tab === "Agents" && agent && <AgentDetail agent={agent} />}
        {setting && <SettingDetail row={setting} />}
        {need && <NeedsDetail item={need} />}
        {tab === "Plan" && <Line>{project.data.plan ?? "Waiting for docs/plan.md…"}</Line>}
      </ScrollPane>
    );
  const hints = [
    "1–9/0/Tab switch",
    "j/k move",
    "Ctrl+D/U half pane",
    "/ find",
    ...(taskTab ? ["a act", "f filter", "F clear"] : []),
    ...(tab === "Agents"
      ? ["n new", "t task", "e ended", "Enter type/change", "Ctrl+] back"]
      : tab === "Needs you"
        ? ["Enter answer", "x withdraw"]
        : ["Enter details"]),
    "l git",
    "? help",
  ];
  const fitted = hints.reduce((line, hint) => {
    const next = line ? `${line} · ${hint}` : hint;
    return next.length + 11 <= width - 2 ? next : line;
  }, "");
  const labels = tabLabels(tab, width - 2, count);
  // The agent terminal shown here: its tmux pane is named on this TUI's own pane, for native
  // tools such as a scrollback viewer (TerminalTools.showPane).
  const shownTerminal = tab === "Agents" ? agent?.terminal : undefined;
  useEffect(() => {
    if (!tools.showPane) return;
    if (!shownTerminal) return tools.showPane(undefined);
    const abort = new AbortController();
    client.call(refs.terminalPane, { terminal: shownTerminal }, { signal: abort.signal }).then(
      (pane) => abort.signal.aborted || tools.showPane?.(pane ?? undefined),
      () => abort.signal.aborted || tools.showPane?.(undefined),
    );
    return () => abort.abort();
  }, [client, tools, shownTerminal]);
  // The popup's backdrop: what is behind it is dimmed while it is open.
  const behind = popup === null ? 1 : 0.35;
  return (
    <box
      width={width}
      height={height}
      backgroundColor={c.background}
      flexDirection="column"
      paddingLeft={1}
      paddingRight={1}
    >
      <box
        border={["bottom"]}
        borderColor={c.border}
        height={3}
        flexShrink={0}
        flexDirection="column"
        opacity={behind}
      >
        <box flexDirection="row" justifyContent="space-between" height={1}>
          <text fg={c.accent} attributes={TextAttributes.BOLD}>
            nts
          </text>
          <text
            fg={!project.online ? c.warning : failing.length ? c.danger : c.muted}
            height={1}
            wrapMode="none"
            truncate
          >
            {!project.online
              ? "disconnected"
              : failing.length
                ? `${failing.length} source(s) failing`
                : (overview?.repo ?? "connecting…")}
          </text>
        </box>
        <box flexDirection="row" height={1} gap={width < 100 ? 1 : 2}>
          {tabs.map((name, i) => (
            <text
              key={name}
              fg={tab === name ? selection.text : c.text}
              bg={tab === name ? selection.background : c.background}
              height={1}
              flexShrink={0}
              attributes={tab === name ? TextAttributes.BOLD | selection.attributes : 0}
              onMouseDown={() => switchTab(name)}
            >
              {labels[i]}
            </text>
          ))}
        </box>
      </box>
      <box flexGrow={1} minHeight={0} paddingTop={1} opacity={behind}>
        {help ? (
          <ScrollPane focused scrollRef={detail}>
            <Section title="Navigation">
              <Line>1–9/0/Tab/Shift+Tab switch · j/k/arrows move · Enter details · Esc back</Line>
              <Line>
                gg/G first/last · gt/gT next/previous · Ctrl+D/U half a pane · Ctrl+F/B full pane
              </Line>
              <Line>/ find with fzf · f filter tasks · F clear filters</Line>
            </Section>
            <Section title="Native agents">
              <Line>
                n opens Codex, Claude Code or a shell · Enter takes input · Ctrl+] returns
              </Line>
              <Line>Closing Signalbox leaves the native program running.</Line>
            </Section>
            <Section title="Local tools">
              <Line>l lazygit in the selected workspace · L lazygit on main · q/Ctrl+Q quit</Line>
            </Section>
            {/* The status words, from the glossary the lists, filters and web share. */}
            {(
              [
                ["Issue states", issueStates],
                ["Task progress", { ...taskConditions, ...taskPhases }],
                ["Agent states", agentStates],
              ] as const
            ).map(([title, terms]) => (
              <Section key={title} title={title}>
                {Object.values(terms).map((term) => (
                  <Line key={term.label}>
                    <span fg={c.text}>{term.label}</span>
                    <span fg={c.muted}>{`  ${term.meaning}`}</span>
                  </Line>
                ))}
              </Section>
            ))}
            {failing.length > 0 && (
              <Section title="Source errors">
                {failing.map((failure) => (
                  <Line key={failure} color={c.danger}>
                    {failure}
                  </Line>
                ))}
              </Section>
            )}
          </ScrollPane>
        ) : split ? (
          <ListDetail
            width={width}
            focus={focus}
            listWidth={
              tab === "Agents" ? Math.min(44, Math.floor(width * 0.28)) : Math.floor(width * 0.55)
            }
            list={list}
            detail={content}
          />
        ) : (
          content
        )}
      </box>
      <box
        height={3}
        flexShrink={0}
        border={["top"]}
        borderColor={c.border}
        flexDirection="column"
        opacity={behind}
      >
        <text
          fg={project.data.settings?.paused || project.data.settings?.error ? c.warning : c.muted}
          height={1}
          wrapMode="none"
          truncate
        >{`${overview?.repo ?? "GitHub"}${project.data.settings ? ` · ${settingsLine(project.data.settings)}` : ""} · ${project.data.issues.length} open issues · ${agents.length} native terminals · ${overview?.root ?? "connecting…"}`}</text>
        <text
          fg={project.error || failing.length ? c.danger : message ? c.warning : c.muted}
          height={1}
          wrapMode="none"
          truncate
        >
          {tool ?? message ?? project.error ?? failing[0] ?? `${fitted} · q quit`}
        </text>
      </box>
      {popup !== null && (
        <box
          position="absolute"
          top={2}
          left={3}
          width={Math.max(20, width - 6)}
          height={Math.max(8, height - 6)}
          zIndex={10}
          border
          borderColor={c.accent}
          backgroundColor={c.background}
          flexDirection="column"
          paddingLeft={1}
          paddingRight={1}
          title={
            popup.kind === "task"
              ? ` #${popup.issue} · Escape or t closes · j/k scroll `
              : ` ${plural(endedAgents.length, "ended session")} · Escape or e closes · j/k scroll `
          }
        >
          <ScrollPane scrollRef={popupScroll} focused>
            {popup.kind === "task" ? (
              <TaskPopup
                number={popup.issue}
                issue={project.data.issues.find((issue) => issue.number === popup.issue)}
                assignment={
                  [...project.data.assignments]
                    .filter((assignment) => assignment.issue === popup.issue)
                    // The live task first, then the latest.
                    .sort(
                      (a, b) =>
                        Number(b.status === "active") - Number(a.status === "active") ||
                        b.acceptedAt - a.acceptedAt,
                    )[0]
                }
              />
            ) : (
              // The most recently ended first.
              [...endedAgents].reverse().map((item) => (
                <box key={item.id} flexDirection="column" flexShrink={0} marginBottom={1}>
                  <Line color={toneColor(agentTone(item.state))}>{`○ ${item.name}`}</Line>
                  {agentDetails(item).map((line, index) => (
                    <Line key={index} color={c.muted}>{`  ${line}`}</Line>
                  ))}
                </box>
              ))
            )}
          </ScrollPane>
        </box>
      )}
    </box>
  );
}
