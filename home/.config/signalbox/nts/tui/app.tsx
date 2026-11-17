/** @jsxImportSource @opentui/react */
import { useRef, useState } from "react";
import { useKeyboard, useTerminalDimensions } from "@opentui/react";
import { TextAttributes } from "@opentui/core";
import type { ScrollBoxRenderable } from "@opentui/core";
import { useProject } from "../shared/use-project.ts";
import { operationsLine, tabs } from "../shared/model.ts";
import type { Tab } from "../shared/model.ts";
import { terminalConfig } from "./config.ts";
import { describeFilters, filterOptions, matches, toggleFilter } from "./filters.ts";
import type { Chosen } from "./filters.ts";
import type { TerminalTools } from "./tools.ts";
import { Heading, Line, ListDetail, ListRow, ScrollPane, Section } from "./components.tsx";
import { AgentDetail, NowView, TaskDetail, agentColor, stateColor, stateMark } from "./views.tsx";
import { colors as c } from "./theme.ts";
import { useSelection } from "./selection.tsx";
import { isNtsPage, tableRows, cellText } from "../status/model.ts";
import { NtsContent } from "./nts.tsx";
import { SettingDetail, settingChoice, settingRows } from "./settings.tsx";
import type { SettingRow } from "./settings.tsx";
import { tabLabels } from "./navigation.ts";
import { navigationKey } from "./keyboard.ts";
import { NativeTerminal } from "./terminal.tsx";

export function TerminalApp({ onExit, tools }: { onExit(): void; tools: TerminalTools }) {
  const selection = useSelection(),
    project = useProject(),
    { width, height } = useTerminalDimensions();
  const [tab, setTab] = useState<Tab>("Now"),
    [focus, setFocus] = useState<"list" | "detail">("list");
  const [selected, setSelected] = useState<Partial<Record<Tab, string>>>({});
  const [chosen, setChosen] = useState<Chosen>(new Map());
  const [message, setMessage] = useState<string>(),
    [tool, setTool] = useState<string>();
  const [help, setHelp] = useState(false),
    [terminalRequest, setTerminalRequest] = useState(0);
  const nativeInput = useRef(false),
    borrowing = useRef(false),
    afterG = useRef(false);
  const listScroll = useRef<ScrollBoxRenderable>(null),
    detail = useRef<ScrollBoxRenderable>(null);
  const { overview } = project.data;
  // Ended sessions stay in the host's record; the list hides them until `e` shows them.
  const [showEnded, setShowEnded] = useState(false);
  const ended = project.data.agents.filter((item) => item.state === "ended").length;
  const agents = showEnded
    ? project.data.agents
    : project.data.agents.filter((item) => item.state !== "ended");
  // The Agents tab ends with the settings its launches follow.
  const settings = settingRows(project);
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
  const taskTab = tab === "Tasks" || tab === "Needs you";
  const tasks = project.data.issues.filter(
    (issue) =>
      (tab === "Needs you" ? issue.state === "needs-user" : issue.state !== "needs-user") &&
      matches(terminalConfig.taskFacets, chosen, issue),
  );
  const nts = tab === "Health" || isNtsPage(tab);
  const report =
    tab === "Health" ? project.nts.health : isNtsPage(tab) ? project.nts.page(tab) : undefined;
  const evidence = tableRows(report);
  const items = taskTab ? tasks : tab === "Agents" ? [...agents, ...settings] : evidence;
  const currentId = items.find((item) => item.id === selected[tab])?.id ?? items[0]?.id;
  const task = tasks.find((item) => item.id === currentId),
    agent = agents.find((item) => item.id === currentId),
    setting = tab === "Agents" ? settings.find((item) => item.id === currentId) : undefined;
  const split = taskTab || tab === "Agents";
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
    const choice = settingChoice(row, project);
    if (!choice) return;
    void local(row.label, async () => {
      const index = await tools.find(choice.lines, row.label);
      if (index !== undefined) await choice.apply(index);
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
  function filter() {
    const options = filterOptions(terminalConfig.taskFacets, chosen, project.data.issues),
      clear = chosen.size > 0;
    const lines = [
      ...(clear ? ["✕ clear the filters"] : []),
      ...options.map(
        (option) => `${option.selected ? "✓" : "·"} ${option.label}  (${option.count})`,
      ),
    ];
    void local("Filter Tasks", async () => {
      const picked = await tools.find(lines, "Filter Tasks");
      if (picked === undefined) return;
      if (clear && picked === 0) setChosen(new Map());
      else {
        const option = options[picked - (clear ? 1 : 0)];
        if (option) setChosen((old) => toggleFilter(old, option.facet, option.value));
      }
      setFocus("list");
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
    if (key.name === "e" && tab === "Agents") {
      setShowEnded((value) => !value);
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
  const label = describeFilters(terminalConfig.taskFacets, chosen);
  const count = (name: Tab) =>
    name === "Tasks"
      ? tasks.length
      : name === "Agents"
        ? agents.length
        : name === "Needs you"
          ? project.data.issues.filter((issue) => issue.state === "needs-user").length
          : undefined;
  const groupOf = (issue: (typeof tasks)[number]) =>
    `${issue.stream}${issue.group ? ` › ${issue.group}` : ""}`;
  const list = (
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
          : `${agents.length} agents${ended ? ` · ${ended} ended ${showEnded ? "shown" : "hidden"} (e)` : ""}`}
      </Line>
      {taskTab &&
        [...new Set(tasks.map(groupOf))].map((group) => (
          <box key={group} flexDirection="column" marginTop={1} flexShrink={0}>
            <Heading>{group}</Heading>
            {tasks
              .filter((issue) => groupOf(issue) === group)
              .map((issue) => (
                <ListRow
                  key={issue.id}
                  id={`row-${issue.id}`}
                  selected={issue.id === currentId}
                  focused={focus === "list"}
                  label={`${stateMark(issue.state)} #${issue.number} ${issue.title}`}
                  end={issue.claims?.[0]?.what ?? issue.state}
                  color={stateColor(issue.state)}
                  onSelect={() => choose(issue.id)}
                />
              ))}
          </box>
        ))}
      {tab === "Agents" &&
        agents.map((item) => (
          <box key={item.id} id={`row-${item.id}`} flexDirection="column" flexShrink={0}>
            <ListRow
              id={`label-${item.id}`}
              selected={item.id === currentId}
              focused={focus === "list"}
              label={`● ${item.name}`}
              end={item.state}
              color={agentColor(item.state)}
              onSelect={() => choose(item.id)}
            />
            <text
              fg={c.muted}
              height={1}
              wrapMode="none"
              truncate
            >{`  ${item.role} · ${item.path ?? "native terminal"}`}</text>
          </box>
        ))}
      {tab === "Agents" && settings.length > 0 && (
        <box flexDirection="column" marginTop={1} flexShrink={0}>
          <Heading>Settings · Enter changes</Heading>
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
      )}
      {!items.length && (
        <Line color={c.muted}>
          {taskTab
            ? "No matching tasks. f changes filters; F clears them."
            : "No native terminals. n opens a session."}
        </Line>
      )}
    </ScrollPane>
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
        {taskTab && task && <TaskDetail issue={task} />}
        {tab === "Agents" && agent && <AgentDetail agent={agent} />}
        {setting && <SettingDetail row={setting} />}
        {tab === "Plan" && <Line>{project.data.plan ?? "Waiting for docs/plan.md…"}</Line>}
      </ScrollPane>
    );
  const hints = [
    "1–9/0/Tab switch",
    "j/k move",
    "Ctrl+D/U half pane",
    "/ find",
    ...(taskTab ? ["f filter", "F clear"] : []),
    ...(tab === "Agents"
      ? ["n new", "e ended", "Enter type/change", "Ctrl+] back"]
      : ["Enter details"]),
    "l git",
    "? help",
  ];
  const fitted = hints.reduce((line, hint) => {
    const next = line ? `${line} · ${hint}` : hint;
    return next.length + 11 <= width - 2 ? next : line;
  }, "");
  const labels = tabLabels(tab, width - 2, count);
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
      <box flexGrow={1} minHeight={0} paddingTop={1}>
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
      <box height={3} flexShrink={0} border={["top"]} borderColor={c.border} flexDirection="column">
        <text
          fg={project.data.operations?.paused ? c.warning : c.muted}
          height={1}
          wrapMode="none"
          truncate
        >{`${overview?.repo ?? "GitHub"}${project.data.operations ? ` · ${operationsLine(project.data.operations)}` : ""} · ${project.data.issues.length} open issues · ${agents.length} native terminals · ${overview?.root ?? "connecting…"}`}</text>
        <text
          fg={project.error || failing.length ? c.danger : message ? c.warning : c.muted}
          height={1}
          wrapMode="none"
          truncate
        >
          {tool ?? message ?? project.error ?? failing[0] ?? `${fitted} · q quit`}
        </text>
      </box>
    </box>
  );
}
