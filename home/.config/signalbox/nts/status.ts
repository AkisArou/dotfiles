// nts's status JSON, checked before it is read. Within a version nts only adds
// fields; removing or renaming one bumps `version`. So a version this
// configuration does not know means the shape changed: it is said plainly, as
// the source's error, rather than read wrong.

/** The status, if it is of a kind and a version this configuration reads; otherwise an error saying what changed. */
export function checked<T>(json: unknown, what: string, versions: readonly number[], kind?: string): T {
  const status = json as { readonly version?: unknown; readonly kind?: unknown } | null;
  if (!status || typeof status !== "object") throw new Error(`nts's ${what} status is not an object`);
  if (kind !== undefined && status.kind !== kind) throw new Error(`nts's ${what} status is of kind ${String(status.kind)}, not ${kind}`);
  if (typeof status.version !== "number" || !versions.includes(status.version)) {
    throw new Error(`nts's ${what} status changed shape: version ${String(status.version)}, where this configuration reads ${versions.join(" or ")} (~/.config/signalbox/nts/)`);
  }
  return json as T;
}

/** nts's issues as signalbox's GitHub source names them: a claim names its task on it. */
export const NTS_ISSUES = "github-issues:AkisArou/nts";

/** The issue a branch belongs to, by nts's branch rule (tooling/tasks/branch.ts): `<N>` or `<N>-<slug>`. */
export function issueOfBranch(branch: string | null | undefined): string | undefined {
  return branch ? /^([1-9][0-9]*)(?:-[a-z0-9]+(?:-[a-z0-9]+)*)?$/.exec(branch)?.[1] : undefined;
}
