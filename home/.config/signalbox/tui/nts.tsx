/** @jsxImportSource @opentui/react */
import { TextAttributes } from "@opentui/core";
import { cellText, isNtsPage, tableRows } from "../nts/model.ts";
import type { Color, Table } from "../nts/model.ts";
import type { Project } from "../shared/use-project.ts";
import type { Tab } from "../shared/model.ts";
import { Heading, Line, Section } from "./components.tsx";
import { useSelection } from "./selection.tsx";
import { colors as c } from "./theme.ts";

const color = (name: Color | undefined) =>
  name === "red"
    ? c.danger
    : name === "green"
      ? c.success
      : name === "yellow"
        ? c.warning
        : name === "gray"
          ? c.muted
          : name === "blue"
            ? c.info
            : name === "cyan"
              ? c.accent
              : c.text;

function columnWidths(table: Table, width: number) {
  const natural = table.columns.map((column) =>
    Math.max(column.label.length, 1, ...table.rows.map((row) => cellText(row[column.key]).length)),
  );
  const room = Math.max(table.columns.length, width - 2 * (table.columns.length - 1));
  const widths = natural.map((size, index) => Math.min(size, index === 0 ? 16 : 6));
  let remaining = room - widths.reduce((sum, size) => sum + size, 0);
  while (remaining > 0) {
    let changed = false;
    for (let index = 0; index < widths.length && remaining > 0; index++)
      if (widths[index]! < natural[index]!) {
        widths[index]!++;
        remaining--;
        changed = true;
      }
    if (!changed) break;
  }
  // Very small terminals still render without overflowing the containing pane.
  while (remaining < 0) {
    const largest = widths.indexOf(Math.max(...widths));
    if (widths[largest]! <= 1) break;
    widths[largest]!--;
    remaining++;
  }
  return widths;
}
function DataTable({
  table,
  width,
  selected,
  onSelect,
}: {
  table: Table;
  width: number;
  selected?: string;
  onSelect(id: string): void;
}) {
  const selection = useSelection(),
    widths = columnWidths(table, width);
  const rows = tableRows({ tables: [table], gauges: [] });
  return (
    <box flexDirection="column" marginBottom={1} flexShrink={0}>
      <Heading>{table.title}</Heading>
      {table.facts?.map(([label, value]) => (
        <Line key={label} color={c.muted}>{`${label}: ${value}`}</Line>
      ))}
      <box flexDirection="row" gap={2} height={1} flexShrink={0}>
        {table.columns.map((column, index) => (
          <text
            key={column.key}
            width={widths[index]}
            height={1}
            fg={c.muted}
            wrapMode="none"
            truncate
            textAlign={column.align}
          >
            {column.label}
          </text>
        ))}
      </box>
      {rows.map((item) => {
        const active = item.id === selected;
        return (
          <box
            key={item.id}
            id={`row-${item.id}`}
            flexDirection="row"
            gap={2}
            height={1}
            flexShrink={0}
            onMouseDown={() => onSelect(item.id)}
          >
            {table.columns.map((column, index) => {
              const value = item.row[column.key],
                style = value && typeof value === "object" ? value : undefined;
              return (
                <text
                  key={column.key}
                  width={widths[index]}
                  height={1}
                  wrapMode="none"
                  truncate
                  textAlign={column.align}
                  fg={active ? selection.text : color(style?.color)}
                  bg={active ? selection.background : c.background}
                  attributes={
                    (active || style?.bold ? TextAttributes.BOLD : 0) |
                    (active ? selection.attributes : 0)
                  }
                >
                  {cellText(value)}
                </text>
              );
            })}
          </box>
        );
      })}
      {!rows.length && <Line color={c.muted}>None.</Line>}
    </box>
  );
}

export function NtsContent({
  project,
  page,
  width,
  selected,
  details,
  onSelect,
}: {
  project: Project;
  page: Tab;
  width: number;
  selected?: string;
  details: boolean;
  onSelect(id: string): void;
}) {
  const reading = isNtsPage(page) ? project.nts.readings[page] : undefined;
  const report =
    page === "Health" ? project.nts.health : isNtsPage(page) ? project.nts.page(page) : undefined;
  const row = tableRows(report).find((item) => item.id === selected);
  return (
    <>
      <Line color={c.warning}>
        {`Read-only NTS observations${project.nts.root ? ` · ${project.nts.root}` : ""}`}
      </Line>
      {reading?.status === "error" && (
        <Line
          color={c.danger}
        >{`${reading.error.message}${report ? " · showing the last good reading" : ""}`}</Line>
      )}
      {page === "Health" &&
        Object.entries(project.nts.readings)
          .filter(([, value]) => value.status === "error")
          .map(([name]) => (
            <Line
              key={name}
              color={c.danger}
            >{`${name} unavailable; its health metrics may be stale.`}</Line>
          ))}
      <box height={1} />
      {!report ? (
        <Line color={c.muted}>
          {reading?.status === "error"
            ? "NTS source is unavailable."
            : "Waiting for NTS observations…"}
        </Line>
      ) : details && row ? (
        <Section title={`${row.table.title} · selected row`}>
          {row.table.columns.map((column) => (
            <Section key={column.key} title={column.label || column.key}>
              <Line>{cellText(row.row[column.key])}</Line>
            </Section>
          ))}
          {Object.entries(row.row)
            .filter(([key]) => !row.table.columns.some((column) => column.key === key))
            .map(([key, value]) => (
              <Line key={key}>{`${key}: ${cellText(value)}`}</Line>
            ))}
          {row.table.facts?.map(([label, value]) => (
            <Line key={label}>{`${label}: ${value}`}</Line>
          ))}
        </Section>
      ) : (
        report.tables.map((table) => (
          <DataTable
            key={table.id}
            table={table}
            width={width}
            selected={selected}
            onSelect={onSelect}
          />
        ))
      )}
    </>
  );
}
