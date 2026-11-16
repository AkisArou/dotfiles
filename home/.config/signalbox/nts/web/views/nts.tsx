import { ntsPages, gaugeState, cellText } from "../../status/model.ts";
import type { NtsPage, Reading, TableCell } from "../../status/model.ts";
import { reportOf } from "../../status/use-nts.ts";
import type { Project } from "../../shared/use-project.ts";
import { Empty, Section } from "../components.tsx";

function Cell({ value }: { value: TableCell | undefined }) {
  const style = value && typeof value === "object" ? value : undefined;
  return (
    <span
      className={style?.color ? `nts-${style.color}` : undefined}
      style={style?.bold ? { fontWeight: 600 } : undefined}
    >
      {cellText(value)}
    </span>
  );
}
function Provenance({ report }: { report: Reading | undefined }) {
  return (
    <div className="notice nts-provenance" role="status">
      {report ? `NTS observations · ${report.root}` : "Waiting for NTS observations…"}
      {report && (
        <span className="subtle">Read {new Date(report.readAt).toLocaleTimeString()}</span>
      )}
    </div>
  );
}
export function NtsView({ project, page }: { project: Project; page: NtsPage }) {
  const reading = project.nts.readings[page],
    report = reportOf(reading);
  return (
    <div className="nts-view">
      <Provenance report={report} />
      {reading.status === "error" && (
        <div className="notice danger" role="alert">
          {reading.error.message}
          {report ? " · Showing the last successful reading." : ""}
        </div>
      )}
      {!report ? (
        <Empty>
          {reading.status === "error" ? "NTS source is unavailable." : "Loading observations…"}
        </Empty>
      ) : (
        report.tables.map((table) => (
          <Section key={table.id} title={table.title}>
            {table.facts?.length ? (
              <dl className="nts-facts">
                {table.facts.map(([label, text]) => (
                  <div key={label}>
                    <dt>{label}</dt>
                    <dd>{text}</dd>
                  </div>
                ))}
              </dl>
            ) : null}
            <div className="table-scroll">
              <table>
                <thead>
                  <tr>
                    {table.columns.map((column) => (
                      <th
                        key={column.key}
                        className={column.align === "right" ? "nts-number" : undefined}
                      >
                        {column.label}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {table.rows.map((row, index) => (
                    <tr key={table.key ? cellText(row[table.key]) : index}>
                      {table.columns.map((column) => (
                        <td
                          key={column.key}
                          className={column.align === "right" ? "nts-number" : undefined}
                        >
                          <Cell value={row[column.key]} />
                        </td>
                      ))}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            {!table.rows.length && <Empty>No entries.</Empty>}
          </Section>
        ))
      )}
    </div>
  );
}
export function NtsHealth({ project }: { project: Project }) {
  const report = ntsPages.flatMap((page) => {
    const value = project.nts.page(page);
    return value ? [value] : [];
  })[0];
  const gauges = project.nts.health.gauges;
  return (
    <div className="nts-view">
      <Provenance report={report} />
      {ntsPages.map((page) =>
        project.nts.readings[page].status === "error" ? (
          <div key={page} className="notice danger" role="alert">
            {page} is unavailable; its metrics may be stale.
          </div>
        ) : null,
      )}
      {!gauges.length ? (
        <Empty>No NTS metrics have been received.</Empty>
      ) : (
        [...new Set(gauges.map((gauge) => gauge.section ?? "NTS"))].map((section) => (
          <Section key={section} title={section}>
            <div className="table-scroll">
              <table>
                <thead>
                  <tr>
                    <th>Metric</th>
                    <th>State</th>
                    <th>Reading</th>
                    <th>Context</th>
                  </tr>
                </thead>
                <tbody>
                  {gauges
                    .filter((gauge) => (gauge.section ?? "NTS") === section)
                    .map((gauge) => (
                      <tr key={gauge.id}>
                        <td>{gauge.label}</td>
                        <td
                          className={`nts-${gaugeState(gauge) === "red" ? "red" : gaugeState(gauge) === "amber" ? "yellow" : "green"}`}
                        >
                          {gaugeState(gauge)}
                        </td>
                        <td>
                          {gauge.text ?? `${gauge.value}${gauge.unit ? ` ${gauge.unit}` : ""}`}
                        </td>
                        <td>{gauge.why ?? ""}</td>
                      </tr>
                    ))}
                </tbody>
              </table>
            </div>
          </Section>
        ))
      )}
    </div>
  );
}
