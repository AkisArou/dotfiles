// The NTS report views (Landing, Full check, Benchmarks, Conformance, Machine)
// and Health. A report is facts and tables: facts as a metadata list, rows as a
// compact table whose cells keep their source's colour.
import { MetadataList, MetadataListItem } from "@astryxdesign/core/MetadataList";
import { Table, proportional } from "@astryxdesign/core/Table";
import { Text } from "@astryxdesign/core/Text";
import { Timestamp } from "@astryxdesign/core/Timestamp";
import { ntsPages, gaugeState, cellText } from "../../status/model.ts";
import { gaugeTone } from "../../shared/model.ts";
import type { NtsPage, Reading, TableCell } from "../../status/model.ts";
import { reportOf } from "../../status/use-nts.ts";
import type { Project } from "../../shared/use-project.ts";
import { Empty, Section, Status } from "../components.tsx";

function Cell({ value }: { value: TableCell | undefined }) {
  const style = value && typeof value === "object" ? value : undefined;
  const text = cellText(value);
  return (
    <span
      className={style?.color ? `nts-${style.color}` : undefined}
      style={style?.bold ? { fontWeight: 600 } : undefined}
      title={text}
    >
      {text}
    </span>
  );
}

/** Where the reading came from and when, as one quiet line. */
function ReadAt({ report }: { report: Reading | undefined }) {
  return report ? (
    <Text type="supporting" color="secondary">
      {report.root} · read <Timestamp value={report.readAt / 1000} format="relative" isLive />
    </Text>
  ) : (
    <Text type="supporting" color="secondary">
      Waiting for NTS readings…
    </Text>
  );
}

export function NtsView({ project, page }: { project: Project; page: NtsPage }) {
  const reading = project.nts.readings[page],
    report = reportOf(reading);
  return (
    <>
      <ReadAt report={report} />
      {reading.status === "error" && (
        <Text type="supporting" className="nts-red">
          {reading.error.message}
          {report ? " · showing the last good reading" : ""}
        </Text>
      )}
      {!report ? (
        <Empty>{reading.status === "error" ? "This NTS source is unavailable" : "Loading…"}</Empty>
      ) : (
        report.tables.map((table) => {
          // A key/value table (two columns, no header of its own) is facts.
          const facts =
            table.facts ??
            (table.columns.length === 2 && table.rows.length <= 12 && !table.columns[0]!.label
              ? table.rows.map(
                  (row) =>
                    [
                      cellText(row[table.columns[0]!.key]),
                      cellText(row[table.columns[1]!.key]),
                    ] as const,
                )
              : undefined);
          const rows = table.rows.map((row, index) => ({
            ...Object.fromEntries(table.columns.map((column) => [column.key, row[column.key]])),
            __id: table.key ? cellText(row[table.key]) : String(index),
          }));
          return (
            <Section
              key={table.id}
              title={table.title}
              count={facts ? undefined : table.rows.length}
            >
              {facts && (
                <div className="panel-body">
                  <MetadataList columns="multi">
                    {facts.map(([label, text]) => (
                      <MetadataListItem key={label} label={label}>
                        {text}
                      </MetadataListItem>
                    ))}
                  </MetadataList>
                </div>
              )}
              {!facts &&
                (rows.length ? (
                  <Table
                    data={rows}
                    idKey="__id"
                    density="compact"
                    dividers="rows"
                    hasHover
                    textOverflow="truncate"
                    columns={table.columns.map((column) => ({
                      key: column.key,
                      header: column.label,
                      align: column.align === "right" ? ("end" as const) : ("start" as const),
                      width: proportional(column.align === "right" ? 1 : 2, { minWidth: 72 }),
                      renderCell: (row: Record<string, unknown>) => (
                        <Cell value={row[column.key] as TableCell | undefined} />
                      ),
                    }))}
                  />
                ) : (
                  <Empty>No entries</Empty>
                ))}
              {table.facts && rows.length > 0 && (
                <Table
                  data={rows}
                  idKey="__id"
                  density="compact"
                  dividers="rows"
                  textOverflow="truncate"
                  columns={table.columns.map((column) => ({
                    key: column.key,
                    header: column.label,
                    align: column.align === "right" ? ("end" as const) : ("start" as const),
                    renderCell: (row: Record<string, unknown>) => (
                      <Cell value={row[column.key] as TableCell | undefined} />
                    ),
                  }))}
                />
              )}
            </Section>
          );
        })
      )}
    </>
  );
}

const SEVERITY = { red: 0, amber: 1, ok: 2 } as const;

export function NtsHealth({ project }: { project: Project }) {
  const report = ntsPages.flatMap((page) => {
    const value = project.nts.page(page);
    return value ? [value] : [];
  })[0];
  const gauges = project.nts.health.gauges;
  return (
    <>
      <ReadAt report={report} />
      {ntsPages
        .filter((page) => project.nts.readings[page].status === "error")
        .map((page) => (
          <Text key={page} type="supporting" className="nts-red">
            {page} is unavailable; its readings may be stale.
          </Text>
        ))}
      {!gauges.length ? (
        <Empty>No NTS readings yet</Empty>
      ) : (
        [...new Set(gauges.map((gauge) => gauge.section ?? "NTS"))].map((section) => {
          const rows = gauges
            .filter((gauge) => (gauge.section ?? "NTS") === section)
            .sort((a, b) => SEVERITY[gaugeState(a)] - SEVERITY[gaugeState(b)])
            .map((gauge) => ({
              id: gauge.id,
              state: gaugeState(gauge),
              label: gauge.label,
              value: gauge.text ?? `${gauge.value}${gauge.unit ? ` ${gauge.unit}` : ""}`,
              why: gauge.why ?? "",
            }));
          return (
            <Section key={section} title={section} count={rows.length}>
              <Table
                data={rows}
                idKey="id"
                density="compact"
                dividers="rows"
                hasHover
                textOverflow="truncate"
                columns={[
                  {
                    key: "state",
                    header: "",
                    width: proportional(1, { minWidth: 90 }),
                    renderCell: (row) => <Status tone={gaugeTone(row.state)}>{row.state}</Status>,
                  },
                  {
                    key: "label",
                    header: "Reading",
                    width: proportional(3),
                    renderCell: (row) => <span title={row.label}>{row.label}</span>,
                  },
                  {
                    key: "value",
                    header: "Value",
                    width: proportional(3),
                    renderCell: (row) => <span title={row.value}>{row.value}</span>,
                  },
                  {
                    key: "why",
                    header: "Why it matters",
                    width: proportional(4),
                    renderCell: (row) => (
                      <span className="secondary" title={row.why}>
                        {row.why}
                      </span>
                    ),
                  },
                ]}
              />
            </Section>
          );
        })
      )}
    </>
  );
}
