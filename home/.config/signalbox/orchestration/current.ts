import type { Readable } from "signalbox/core";

/** A source's latest good value: the ready value, the last one before an error, or undefined while loading. */
export function current<T>(source: Readable<T>): T | undefined {
  const reading = source.get();
  if (reading.status === "ready") return reading.value;
  if (reading.status === "error") return reading.previous?.value;
  return undefined;
}

/** One source derived from two, updated whenever either changes. */
export function combineReadables<A, B, T>(
  a: Readable<A>,
  b: Readable<B>,
  combine: (a: A, b: B) => T,
): Readable<T> {
  const read = (): import("signalbox/core").Reading<T> => {
    const left = a.get(),
      right = b.get();
    if (left.status === "error") return { status: "error", error: left.error };
    if (right.status === "error") return { status: "error", error: right.error };
    if (left.status !== "ready" || right.status !== "ready") return { status: "loading" };
    return {
      status: "ready",
      value: combine(left.value, right.value),
      observedAt: Math.max(left.observedAt, right.observedAt),
    };
  };
  return {
    get: read,
    subscribe(listener) {
      const first = a.subscribe(listener),
        second = b.subscribe(listener);
      return () => {
        first();
        second();
      };
    },
  };
}
