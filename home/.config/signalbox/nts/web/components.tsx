// The console's shared parts, composed from Astryx: a panel (a Card with a
// quiet one-line header), a status (StatusDot and its word), actions that run
// through the project, and compact empty states.
import type { ReactNode } from "react";
import { Button } from "@astryxdesign/core/Button";
import { Card } from "@astryxdesign/core/Card";
import { EmptyState } from "@astryxdesign/core/EmptyState";
import { HStack } from "@astryxdesign/core/HStack";
import { StatusDot } from "@astryxdesign/core/StatusDot";
import { Table } from "@astryxdesign/core/Table";
import type { TableProps } from "@astryxdesign/core/Table";
import { Text } from "@astryxdesign/core/Text";
import type { Project } from "../shared/use-project.ts";
import { useNarrow } from "./narrow.ts";
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
        <span className="panel-title" title={title}>
          <Text type="label" weight="semibold">
            {title}
          </Text>
        </span>
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

/**
 * A table on a desktop; on a phone, each row a small card of its cells, each header
 * over its value, the first cell as the card's title. Empty cells are left out.
 */
export function ResponsiveTable<T extends Record<string, unknown>>(
  props: TableProps<T> & { data: T[] },
) {
  const narrow = useNarrow();
  if (!narrow) return <Table {...props} />;
  const columns = props.columns ?? [];
  const key = (row: T, index: number) =>
    typeof props.idKey === "function"
      ? String(props.idKey(row))
      : props.idKey
        ? String(row[props.idKey])
        : String(index);
  return (
    <div className="stacked-rows">
      {props.data.map((row, index) => (
        <div key={key(row, index)} className="stacked-row">
          {columns.map((column, at) => {
            const value = column.renderCell
              ? column.renderCell(row)
              : (row[column.key] as ReactNode);
            if (value === null || value === undefined || value === "" || value === false)
              return null;
            return at === 0 ? (
              <div key={column.key} className="stacked-title">
                {value}
              </div>
            ) : (
              <div key={column.key} className="stacked-cell">
                {column.header ? <span className="stacked-label">{column.header}</span> : null}
                <span className="stacked-value">{value}</span>
              </div>
            );
          })}
        </div>
      ))}
    </div>
  );
}
