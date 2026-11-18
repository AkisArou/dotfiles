/** @jsxImportSource @opentui/react */
// What needs attention on Now: nts's readings past their thresholds.
import { gaugeTone } from "@signalbox/kit/shared/protocol";
import { Line, Section } from "@signalbox/kit/tui/components";
import { colors as c } from "@signalbox/kit/tui/theme";
import { toneColor } from "@signalbox/kit/tui/views";
import { gaugeState } from "../status/model.ts";
import type { Project } from "../shared/use-project.ts";

export function Attention({ project }: { project: Project }) {
  return (
    <Section title="Health">
      {project.nts.health.gauges
        .filter((gauge) => gaugeState(gauge) !== "ok")
        .map((gauge) => (
          <Line
            key={`${gauge.section}/${gauge.id}`}
            color={toneColor(gaugeTone(gaugeState(gauge)))}
          >{`${gauge.label}: ${gauge.text ?? gauge.value}${gauge.why ? ` · ${gauge.why}` : ""}`}</Line>
        ))}
      {!project.nts.readAt && <Line color={c.muted}>Waiting for NTS observations…</Line>}
    </Section>
  );
}
