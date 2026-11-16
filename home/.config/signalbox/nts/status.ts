/** NTS owns its status formats and their versions; Signalbox does not prescribe either. */
export function checked<T>(
  value: unknown,
  name: string,
  versions: readonly number[],
  kind?: string,
): T {
  if (!value || typeof value !== "object") throw new Error(`NTS ${name} status is not an object`);
  const status = value as { version?: unknown; kind?: unknown };
  if (kind !== undefined && status.kind !== kind)
    throw new Error(`NTS ${name} status has kind ${String(status.kind)}, expected ${kind}`);
  if (typeof status.version !== "number" || !versions.includes(status.version))
    throw new Error(
      `NTS ${name} status version ${String(status.version)} is unsupported; expected ${versions.join(" or ")}`,
    );
  return value as T;
}
