// Shared project hook: typed sources, independent of the DOM or terminal renderer.
import { useSource } from "signalbox/client/react";
import type { RemoteReading } from "signalbox/core/remote";
import { ntsSources, ntsPages, healthReport } from "./model.ts";
import type { Reading, NtsPage } from "./model.ts";

export function reportOf(reading: RemoteReading<Reading>): Reading | undefined {
  return reading.status === "ready"
    ? reading.value
    : reading.status === "error"
      ? reading.previous
      : undefined;
}
export function useNts() {
  const readings = {
    Benchmarks: useSource(ntsSources.Benchmarks),
    Conformance: useSource(ntsSources.Conformance),
    "Full check": useSource(ntsSources["Full check"]),
    Machine: useSource(ntsSources.Machine),
    Landing: useSource(ntsSources["Landing"]),
  };
  const reports = ntsPages.flatMap((page) => {
    const value = reportOf(readings[page]);
    return value ? [value] : [];
  });
  const health = healthReport(reports.flatMap((report) => report.gauges));
  return {
    readings,
    health,
    page: (page: NtsPage) => reportOf(readings[page]),
    root: reports[0]?.root,
    readAt: reports.length ? Math.min(...reports.map((report) => report.readAt)) : undefined,
  };
}
