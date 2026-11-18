// A trend as the report cells show it: glyphs for the terminal and its range, shared by
// the host's projections and the web page (so it holds no Node code).
const BARS = "▁▂▃▄▅▆▇█";
/** A series as bars, lowest to highest (lower is better for every series here). */
export function sparkline(values: readonly (number | undefined)[]): string {
  const present = values.filter(
    (value): value is number => value !== undefined && Number.isFinite(value),
  );
  if (!present.length) return "";
  const low = Math.min(...present);
  const high = Math.max(...present);
  return values
    .map((value) =>
      value === undefined || !Number.isFinite(value)
        ? " "
        : BARS[high === low ? 3 : Math.round(((value - low) / (high - low)) * (BARS.length - 1))]!,
    )
    .join("");
}
/** A series' range, low–high: a line drawn to its own range says how far it really moved. */
export function seriesRange(values: readonly (number | undefined | null)[]): string {
  const present = values.filter(
    (value): value is number => typeof value === "number" && Number.isFinite(value),
  );
  if (!present.length) return "";
  const low = Math.min(...present);
  const high = Math.max(...present);
  return low === high ? low.toFixed(2) : `${low.toFixed(2)}–${high.toFixed(2)}`;
}
/** A trend in glyphs and its range, for the terminal. */
export const trendText = (values: readonly (number | undefined)[]) =>
  `${sparkline(values)} ${seriesRange(values)}`.trim();
