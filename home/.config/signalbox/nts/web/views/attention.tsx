// What needs attention on Now: nts's readings past their thresholds, worst first.
import { Button } from "@astryxdesign/core/Button";
import { List, ListItem } from "@astryxdesign/core/List";
import { useNavigate } from "@tanstack/react-router";
import { gaugeTone } from "@signalbox/kit/shared/protocol";
import { Empty, Section, Status } from "@signalbox/kit/web/components";
import { gaugeState } from "../../status/model.ts";
import type { Project } from "../../shared/use-project.ts";

const SEVERITY = { red: 0, amber: 1, ok: 2 } as const;

export function Attention({ project }: { project: Project }) {
  const navigate = useNavigate();
  const attention = project.nts.health.gauges
    .filter((gauge) => gaugeState(gauge) !== "ok")
    .sort((a, b) => SEVERITY[gaugeState(a)] - SEVERITY[gaugeState(b)]);
  return (
    <Section
      title="Attention"
      count={attention.length}
      aside={
        <Button
          label="All readings"
          variant="ghost"
          size="sm"
          onClick={() => void navigate({ to: "/health" })}
        >
          All readings
        </Button>
      }
    >
      {attention.length ? (
        <List density="compact" hasDividers edge="padded">
          {attention.map((gauge) => (
            <ListItem
              key={`${gauge.section}/${gauge.id}`}
              startContent={<Status tone={gaugeTone(gaugeState(gauge))}>{""}</Status>}
              label={`${gauge.label}: ${gauge.text ?? `${gauge.value}${gauge.unit ? ` ${gauge.unit}` : ""}`}`}
              description={gauge.why}
            />
          ))}
        </List>
      ) : (
        <Empty>
          {project.nts.readAt ? "No reading crosses its threshold" : "Waiting for NTS readings…"}
        </Empty>
      )}
    </Section>
  );
}
