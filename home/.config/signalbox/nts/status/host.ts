// Project wiring: polling NTS observations and exposing JSON sources through the SDK.
import { resolve } from "node:path";
import { z } from "zod";
import { Cell, capability, provide } from "signalbox/core";
import type { Extension, Readable } from "signalbox/core";
import { expandPath, poll } from "signalbox/core/utils";
import { exposeSource } from "signalbox/host";
import { ntsPages, ntsSources } from "./model.ts";
import type { Reading } from "./model.ts";
import { intervals, liveReaders } from "./readers.ts";

export function ntsObservations(options: { root: string }): Extension {
  const root = resolve(expandPath(options.root));
  const sources = ntsPages.map((page) => ({
    page,
    cell: new Cell<Reading>(),
    key: capability<Readable<Reading>>(`nts-observations/${page}`),
  }));
  const polling: Extension = {
    id: "nts-observations",
    provides: sources.map(({ key, cell }) => provide(key, cell)),
    start(context) {
      const readers = liveReaders(root, context.signal);
      context.afterStart(() => {
        for (const { page, cell } of sources) {
          const loop = poll(
            async () => {
              const report = await readers[page]();
              cell.set({ ...report, root, readAt: Date.now() });
            },
            intervals[page],
            context.signal,
            (error) => cell.fail(error),
          );
          context.onStop(() => loop);
        }
      });
    },
  };
  return {
    id: "nts-dashboard",
    extensions: [
      polling,
      ...sources.map(({ page, key }) =>
        exposeSource({
          ref: ntsSources[page],
          requires: [key],
          output: z.custom<Reading>(),
          resolve: (context) => context.get(key),
        }),
      ),
    ],
  };
}
