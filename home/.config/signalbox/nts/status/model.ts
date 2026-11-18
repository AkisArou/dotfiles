// USER DATA: NTS's presentation DTOs. These are not Signalbox SDK abstractions.
import { source } from "signalbox/core/remote";

export type Color = "red" | "green" | "yellow" | "gray" | "cyan" | "blue";
export type TableCell =
  | string
  | number
  | {
      readonly text: string;
      readonly color?: Color;
      readonly bold?: boolean;
      /** The issue the cell is about: a page may open it. */
      readonly issue?: number;
      /** A trend, oldest first (null where a run had none): the web draws it as a line;
       * the text says it in glyphs for the terminal. */
      readonly series?: readonly (number | null)[];
    };
export type TableRow = Readonly<Record<string, TableCell>>;
export type Table = {
  readonly id: string;
  readonly title: string;
  readonly columns: readonly {
    readonly key: string;
    readonly label: string;
    readonly align?: "left" | "right";
  }[];
  readonly rows: readonly TableRow[];
  readonly key?: string;
  readonly facts?: readonly (readonly [string, string])[];
  readonly at?: number;
};
export type Gauge = {
  readonly id: string;
  readonly label: string;
  readonly section?: string;
  readonly value: number;
  readonly unit?: string;
  readonly text?: string;
  readonly state?: "ok" | "amber" | "red";
  readonly limits?: {
    readonly amber?: number;
    readonly red?: number;
    readonly worse?: "lower" | "higher";
  };
  readonly why?: string;
};
import type { WorkClaim } from "@signalbox/kit/shared/protocol";
export type { WorkClaim };
export type Report = {
  readonly claims?: readonly WorkClaim[];
  readonly tables: readonly Table[];
  readonly gauges: readonly Gauge[];
};
export type Reading = Report & {
  readonly root: string;
  readonly readAt: number;
};
export const ntsPages = ["Benchmarks", "Conformance", "Full check", "Machine", "Landing"] as const;
export type NtsPage = (typeof ntsPages)[number];
export function isNtsPage(value: string): value is NtsPage {
  return ntsPages.some((page) => page === value);
}
export const ntsSources = {
  Benchmarks: source<Reading>("nts.benchmarks"),
  Conformance: source<Reading>("nts.conformance"),
  "Full check": source<Reading>("nts.full-check"),
  Machine: source<Reading>("nts.machine"),
  Landing: source<Reading>("nts.landing"),
} as const;
export function cellText(cell: TableCell | undefined): string {
  return cell === undefined ? "" : typeof cell === "object" ? cell.text : String(cell);
}
export function gaugeState(gauge: Gauge): "ok" | "amber" | "red" {
  if (gauge.state) return gauge.state;
  const crossed = (limit: number | undefined) =>
    limit !== undefined &&
    (gauge.limits?.worse === "lower" ? gauge.value <= limit : gauge.value >= limit);
  return crossed(gauge.limits?.red) ? "red" : crossed(gauge.limits?.amber) ? "amber" : "ok";
}
export function tableRows(report: Report | undefined) {
  return (
    report?.tables.flatMap((table) =>
      table.rows.map((row, index) => ({
        id: `${table.id}/${table.key ? cellText(row[table.key]) : index}`,
        table,
        row,
      })),
    ) ?? []
  );
}
export type NtsRow = ReturnType<typeof tableRows>[number];
export function healthReport(gauges: readonly Gauge[]): Report {
  return {
    gauges,
    tables: [
      {
        id: "nts-health",
        title: "NTS health",
        key: "id",
        columns: [
          { key: "metric", label: "metric" },
          { key: "state", label: "state" },
          { key: "reading", label: "reading" },
          { key: "why", label: "context" },
        ],
        rows: gauges.map((gauge) => ({
          id: `${gauge.section}/${gauge.id}`,
          metric: `${gauge.section ?? "NTS"} · ${gauge.label}`,
          state: {
            text: gaugeState(gauge),
            color:
              gaugeState(gauge) === "red"
                ? "red"
                : gaugeState(gauge) === "amber"
                  ? "yellow"
                  : "green",
          },
          reading: gauge.text ?? `${gauge.value}${gauge.unit ? ` ${gauge.unit}` : ""}`,
          why: gauge.why ?? "",
        })),
      },
    ],
  };
}
