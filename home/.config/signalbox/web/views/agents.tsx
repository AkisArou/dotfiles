import type { Project } from "../../shared/use-project.ts";
import { useEffect, useRef, useState } from "react";
import { ClickableCard } from "@astryxdesign/core/ClickableCard";
import { Badge, Empty, Section } from "../components.tsx";
import { Action } from "../components.tsx";
import { NativeTerminal } from "../terminal.tsx";

export function Agents({ project }: { project: Project }) {
  const [selected, setSelected] = useState<string>();
  const details = useRef<HTMLDivElement>(null);
  const agents = project.data.agents.filter((agent) => agent.state !== "ended" || agent.terminal),
    agent = agents.find((agent) => agent.id === selected) ?? agents[0];
  useEffect(() => {
    if (selected && agent?.id === selected && window.innerWidth <= 1150)
      details.current?.scrollIntoView({ block: "start" });
  }, [selected, agent?.id]);
  return (
    <div className="agents-view">
      <div className="terminal-programs" aria-label="Open a native session">
        {project.terminalPrograms.map((program) => (
          <Action
            key={program.id}
            project={project}
            onClick={async () => {
              const result = await project.commands.startTerminal.run({ program: program.id });
              setSelected(result.id);
            }}
          >{`New ${program.label}`}</Action>
        ))}
      </div>
      <div className="agents-layout">
        <div className="agent-list">
          {agents.map((item) => (
            <ClickableCard
              label={item.name}
              key={item.id}
              className={`agent-item ${agent?.id === item.id ? "active" : ""}`}
              onClick={() => setSelected(item.id)}
            >
              <span className="agent-glyph">{item.role === "coordinator" ? "⌘" : "⌁"}</span>
              <div>
                <strong>{item.name}</strong>
                <span>{item.path ?? "Native terminal"}</span>
              </div>
              <Badge state={item.state}>{item.state}</Badge>
            </ClickableCard>
          ))}
          {!agents.length && <Empty>Sessions appear when work starts.</Empty>}
        </div>
        <div ref={details} className="agent-detail">
          {agent && (
            <Section title={agent.name} aside={<span className="subtle">NATIVE TERMINAL</span>}>
              {agent.terminal ? (
                <NativeTerminal key={agent.terminal} id={agent.terminal} />
              ) : (
                <Empty>No terminal attachment is available for this session.</Empty>
              )}
            </Section>
          )}
        </div>
      </div>
    </div>
  );
}
