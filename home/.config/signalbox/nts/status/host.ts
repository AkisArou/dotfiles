// The NTS dashboard's pages as JSON sources. Landing and Full check project the
// statuses the orchestration already keeps current; the other pages read their
// own NTS commands, and only while someone views them.
import { resolve } from "node:path";
import { z } from "zod";
import { mapReadable, polledSource } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import { expandPath } from "signalbox/core/utils";
import { exposeSource } from "signalbox/host";
import { ntsSources } from "./model.ts";
import type { NtsPage, Reading, Report } from "./model.ts";
import { intervals, liveReaders } from "./readers.ts";
import type { ReadPage } from "./readers.ts";
import { fullCheckReport } from "./full-check.ts";
import type { FullCheckStatus } from "./full-check.ts";
import { landingReport } from "./landing.ts";
import type { LandingStatus } from "./landing.ts";

export function ntsObservations(options: {
  readonly root: string;
  readonly landing: Capability<Readable<LandingStatus>>;
  readonly fullCheck: Capability<Readable<FullCheckStatus>>;
}): Extension {
  const root = resolve(expandPath(options.root));
  const reading = (report: Report, readAt: number): Reading => ({ ...report, root, readAt });
  const page = <T>(
    name: NtsPage,
    source: Capability<Readable<T>>,
    project: (value: T, at: number) => Reading,
  ) =>
    exposeSource({
      ref: ntsSources[name],
      requires: [source],
      output: z.custom<Reading>(),
      resolve: (context) => mapReadable(context.get(source), project),
    });
  const read: readonly ReadPage[] = ["Benchmarks", "Conformance", "Machine"];
  const polled = read.map((name) => ({
    name,
    source: polledSource<Reading>({
      id: `nts-observations/${name}`,
      interval: intervals[name],
      demand: "subscribed",
      read: async (signal) => reading(await liveReaders(root, signal)[name](), Date.now()),
    }),
  }));
  return {
    id: "nts-dashboard",
    extensions: [
      ...polled.map(({ source }) => source.extension),
      ...polled.map(({ name, source }) => page(name, source.key, (value) => value)),
      page("Landing", options.landing, (status, at) => reading(landingReport(status), at)),
      page("Full check", options.fullCheck, (status, at) => reading(fullCheckReport(status), at)),
    ],
  };
}
