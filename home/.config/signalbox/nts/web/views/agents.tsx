// Agents: the sessions (a list beside the selected one's terminal) and the
// settings their launches follow, as two routes under /agents (a task's own
// steps are in the issue drawer). A session working on an issue opens it in the issue drawer;
// ended sessions are kept out of the list and open in a drawer of their own.
import { agentStates } from "../../shared/glossary.ts";
import { agentLines, agentTone } from "../../shared/model.ts";
import type { Agent } from "../../shared/model.ts";
import { useState } from "react";
import type { ReactNode } from "react";
import { useLocation, useNavigate } from "@tanstack/react-router";
import { Button } from "@astryxdesign/core/Button";
import { HStack } from "@astryxdesign/core/HStack";
import { Drawer, DrawerHeader } from "@astryxdesign/core/Drawer";
import { List, ListItem } from "@astryxdesign/core/List";
import { Tab, TabList } from "@astryxdesign/core/TabList";
import type { Project } from "../../shared/use-project.ts";
import { Action, Empty, Section, Status } from "../components.tsx";
import { NativeTerminal } from "../terminal.tsx";
import { useNarrow } from "../narrow.ts";
import { useOverlay } from "../overlay.ts";

const PANES = [
  { path: "/agents", label: "Sessions" },
  { path: "/agents/settings", label: "Settings" },
] as const;

/** The panes' tabs above whichever pane the route shows. */
export function AgentPanes({ children }: { children: ReactNode }) {
  const navigate = useNavigate();
  const { pathname } = useLocation();
  const current =
    PANES.find((pane) => pane.path === pathname.replace(/\/$/, ""))?.path ?? "/agents";
  return (
    <>
      <TabList
        value={current}
        onChange={(path) => void navigate({ to: path })}
        size="sm"
        hasDivider
      >
        {PANES.map((pane) => (
          <Tab key={pane.path} value={pane.path} label={pane.label} />
        ))}
      </TabList>
      {children}
    </>
  );
}

export function Sessions({ project }: { project: Project }) {
  const [selected, setSelected] = useState<string>();
  const [endedOpen, setEndedOpen] = useState(false);
  // The most recently ended first.
  const ended = project.data.agents.filter((agent) => agent.state === "ended").reverse();
  const agents = project.data.agents.filter((agent) => agent.state !== "ended"),
    agent = agents.find((item) => item.id === selected) ?? agents[0];
  const lines = (item: Agent) => (
    <>
      {agentLines(item, project.data.issues).map((line) => (
        <span key={line} className="line">
          {line}
        </span>
      ))}
    </>
  );
  const narrow = useNarrow();
  const overlay = useOverlay();
  const openScreen = (id: string) => overlay.open({ screen: id });
  const openIssue = (issue: number) => overlay.open({ issue });
  return (
    <div className="split">
      <Section
        title="Sessions"
        count={agents.length}
        aside={
          ended.length > 0 && (
            <Button
              label={`Show the ${ended.length} ended sessions`}
              variant="ghost"
              size="sm"
              onClick={() => setEndedOpen(true)}
            >
              {`${ended.length} ended`}
            </Button>
          )
        }
      >
        {agents.length ? (
          <List density="compact" hasDividers edge="padded">
            {agents.map((item) => (
              <ListItem
                key={item.id}
                label={item.name}
                description={lines(item)}
                endContent={
                  <Status tone={agentTone(item.state)} title={agentStates[item.state]?.meaning}>
                    {item.state}
                  </Status>
                }
                isSelected={agent?.id === item.id}
                onClick={() =>
                  narrow && item.terminal ? openScreen(item.id) : setSelected(item.id)
                }
              />
            ))}
          </List>
        ) : (
          <Empty>Sessions appear when work starts</Empty>
        )}
        <div className="panel-body">
          {project.terminalPrograms.map((program) => (
            <Action
              key={program.id}
              project={project}
              quiet
              onClick={async () => {
                const result = await project.commands.startTerminal.run({ program: program.id });
                setSelected(result.id);
              }}
            >{`New ${program.label}`}</Action>
          ))}
        </div>
      </Section>
      <Drawer
        isOpen={endedOpen}
        onOpenChange={setEndedOpen}
        label="Ended sessions"
        side="end"
        width={560}
        purpose="info"
      >
        <DrawerHeader title={`${ended.length} ended sessions`} onOpenChange={setEndedOpen} />
        <List density="compact" hasDividers edge="padded">
          {ended.map((item) => (
            <ListItem
              key={item.id}
              label={item.name}
              description={lines(item)}
              endContent={
                <Status tone={agentTone(item.state)} title={agentStates[item.state]?.meaning}>
                  {item.state}
                </Status>
              }
            />
          ))}
        </List>
      </Drawer>
      {narrow ? null : agent ? (
        <Section
          title={agent.name}
          aside={
            <HStack gap={3} align="center">
              {agent.issue && (
                <Button
                  label={`Open issue #${agent.issue}`}
                  variant="secondary"
                  size="sm"
                  onClick={() => openIssue(agent.issue!)}
                >
                  {`#${agent.issue}`}
                </Button>
              )}
              <Status tone={agentTone(agent.state)} title={agentStates[agent.state]?.meaning}>
                {agent.state}
              </Status>
              {agent.terminal && (
                <Button
                  label={`Show ${agent.name} full screen, to scroll its transcript and message it`}
                  variant="secondary"
                  size="sm"
                  onClick={() => openScreen(agent.id)}
                >
                  Full screen
                </Button>
              )}
            </HStack>
          }
        >
          {agent.terminal ? (
            <NativeTerminal key={agent.terminal} id={agent.terminal} />
          ) : (
            <Empty>No terminal is attached to this session</Empty>
          )}
        </Section>
      ) : (
        <Section title="Terminal">
          <Empty>Select a session</Empty>
        </Section>
      )}
    </div>
  );
}
