import type { Readable } from "signalbox/core";

/** A source's latest good value: the ready value, the last one before an error, or undefined while loading. */
export function current<T>(source: Readable<T>): T | undefined {
  const reading = source.get();
  if (reading.status === "ready") return reading.value;
  if (reading.status === "error") return reading.previous?.value;
  return undefined;
}
