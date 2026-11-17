// Plan: the project's docs/plan.md, rendered as the Markdown it is.
import { Markdown } from "@astryxdesign/core/Markdown";
import type { Project } from "../../shared/use-project.ts";
import { Empty, Section } from "../components.tsx";

export function Plan({ project }: { project: Project }) {
  return (
    <Section title="docs/plan.md">
      {project.data.plan ? (
        <div className="panel-body">
          <Markdown density="compact" headingLevelStart={2} contentWidth="100ch">
            {project.data.plan}
          </Markdown>
        </div>
      ) : (
        <Empty>Waiting for docs/plan.md…</Empty>
      )}
    </Section>
  );
}
