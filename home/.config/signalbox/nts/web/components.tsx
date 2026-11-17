// The console's shared parts, composed from Astryx: a panel (a Card with a
// quiet one-line header), a status (StatusDot and its word), actions that run
// through the project, and compact empty states.
import type { ReactNode } from "react";
import { Button } from "@astryxdesign/core/Button";
import { Card } from "@astryxdesign/core/Card";
import { EmptyState } from "@astryxdesign/core/EmptyState";
import { HStack } from "@astryxdesign/core/HStack";
import { StatusDot } from "@astryxdesign/core/StatusDot";
import { Text } from "@astryxdesign/core/Text";
import type { Project } from "../shared/use-project.ts";
import type { Tone } from "../shared/model.ts";

const VARIANT: Record<Tone, "success" | "warning" | "error" | "accent" | "neutral"> = {
  good: "success",
  active: "accent",
  attention: "warning",
  bad: "error",
  quiet: "neutral",
  plain: "neutral",
};

/** A state as a dot and its word, in the tone the shared rules give it (as the TUI shows it). */
export function Status({
  tone,
  children,
  title,
}: {
  tone: Tone;
  children: ReactNode;
  /** What the state means (the glossary's), shown on hover. */
  title?: string;
}) {
  const status = (
    <HStack gap={1.5} align="center" className="status">
      <StatusDot variant={VARIANT[tone]} label={typeof children === "string" ? children : tone} />
      <Text
        type="body"
        size="sm"
        color={tone === "quiet" ? "secondary" : "primary"}
        className="nowrap"
      >
        {children}
      </Text>
    </HStack>
  );
  return title ? <span title={title}>{status}</span> : status;
}

/** A panel: a flat Card whose header is one quiet line (title, count, aside). */
export function Section({
  title,
  count,
  aside,
  children,
  className = "",
}: {
  title: string;
  count?: number | string;
  aside?: ReactNode;
  children: ReactNode;
  className?: string;
}) {
  return (
    <Card className={`panel ${className}`} padding={0}>
      <div className="panel-head">
        <Text type="label" weight="semibold">
          {title}
        </Text>
        {count !== undefined && (
          <Text type="supporting" color="secondary">
            {count}
          </Text>
        )}
        {aside && <div className="panel-aside">{aside}</div>}
      </div>
      {children}
    </Card>
  );
}

export function Empty({ children }: { children: string }) {
  return <EmptyState title={children} isCompact headingLevel={3} />;
}

/** A button that runs a host operation; disabled while offline or another runs. */
export function Action({
  project,
  children,
  onClick,
  primary = false,
  quiet = false,
  disabled = false,
}: {
  project: Project;
  children: string;
  onClick(): Promise<unknown>;
  primary?: boolean;
  quiet?: boolean;
  disabled?: boolean;
}) {
  return (
    <Button
      label={children}
      variant={primary ? "primary" : quiet ? "ghost" : "secondary"}
      size="sm"
      isDisabled={!project.online || project.busy || disabled}
      clickAction={async () => {
        await project.perform(onClick);
      }}
    >
      {children}
    </Button>
  );
}
