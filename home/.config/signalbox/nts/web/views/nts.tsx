// The NTS report views (Landing, Full check, Benchmarks, Conformance, Machine)
// and Health. A report is facts and tables: facts as a metadata list, rows as a
// compact table whose cells keep their source's colour.
import { MetadataList, MetadataListItem } from "@astryxdesign/core/MetadataList";
import { proportional } from "@astryxdesign/core/Table";
import { Text } from "@astryxdesign/core/Text";
import { Timestamp } from "@astryxdesign/core/Timestamp";
import { ntsPages, gaugeState, cellText } from "../../status/model.ts";
import { gaugeTone } from "@signalbox/kit/shared/protocol";
import type { NtsPage, Reading, TableCell } from "../../status/model.ts";
import { reportOf } from "../../status/use-nts.ts";
import type { Project } from "../../shared/use-project.ts";
import { Empty, ResponsiveTable, Section, Status } from "@signalbox/kit/web/components";
import { useOverlay } from "@signalbox/kit/web/overlay";

/** A trend as a small line: lower is drawn lower, gaps where a run had no value, and a
 * dot on the newest. */
function Trend({ values, label }: { values: readonly (number | null)[]; label: string }) {
  const present = values.filter((value): value is number => value !== null);
  if (present.length < 2) return <span title={label}>{label}</span>;
  const width = 104;
  const height = 22;
  const pad = 3;
  const low = Math.min(...present);
  const high = Math.max(...present);
  const x = (index: number) => pad + (index * (width - 2 * pad)) / Math.max(1, values.length - 1);
  const y = (value: number) =>
    high === low ? height / 2 : height - pad - ((value - low) / (high - low)) * (height - 2 * pad);
  const segments: string[] = [];
  let current = "";
  values.forEach((value, index) => {
    if (value === null) {
      if (current) segments.push(current);
      current = "";
      return;
    }
    current += `${current ? "L" : "M"}${x(index).toFixed(1)},${y(value).toFixed(1)}`;
  });
  if (current) segments.push(current);
  const lastIndex = values.findLastIndex((value) => value !== null);
  const last = values[lastIndex] as number;
  return (
    <svg
      className="nts-trend"
      width={width}
      height={height}
      viewBox={`0 0 ${width} ${height}`}
      role="img"
      aria-label={label}
    >
      <title>{label}</title>
      {segments.map((path) => (
        <path
          key={path}
          d={path}
          fill="none"
          stroke="currentColor"
          strokeWidth={1.6}
          strokeLinejoin="round"
          strokeLinecap="round"
        />
      ))}
      <circle cx={x(lastIndex)} cy={y(last)} r={2.6} fill="currentColor" />
    </svg>
  );
}

function Cell({ value }: { value: TableCell | undefined }) {
  const overlay = useOverlay();
  const style = value && typeof value === "object" ? value : undefined;
  const text = cellText(value);
  if (style?.series)
    return (
      <span className={style.color ? `nts-${style.color}` : undefined}>
        <Trend
          values={style.series}
          label={style.series.map((each) => (each === null ? "—" : each.toFixed(2))).join(" → ")}
        />
      </span>
    );
  // A cell about an issue opens it, as a task does anywhere.
  if (style?.issue)
    return (
      <button
        type="button"
        className={`cell-link${style.color ? ` nts-${style.color}` : ""}`}
        title={`Open #${style.issue}`}
        onClick={() => overlay.open({ issue: style.issue! })}
      >
        {text}
      </button>
    );
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
                  <ResponsiveTable
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
                <ResponsiveTable
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
              <ResponsiveTable
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
