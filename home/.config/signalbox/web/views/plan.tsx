import type { Project } from "../../shared/use-project.ts";
import { Section } from "../components.tsx";

export function Plan({ project }: { project: Project }) {
  return (
    <Section title="docs/plan.md" aside={<span className="subtle">PROJECT-OWNED FILE</span>}>
      <pre className="plan-document">{project.data.plan}</pre>
    </Section>
  );
}
