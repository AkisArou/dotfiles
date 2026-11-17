// NTS's observed work, not a Signalbox assignment or ownership policy.
import type { Agent, Issue } from "./model.ts";
import type { WorkClaim } from "../status/model.ts";

/** A tree used within this long counts as being worked in. */
const WORKING_MS = 3_600_000;

export function claimedIssues(
  issues: readonly Issue[],
  agents: readonly Agent[],
  claims: readonly WorkClaim[],
): Issue[] {
  return issues.map((issue) => {
    const on = claims.filter((claim) => claim.issue === issue.number);
    if (!on.length) return issue;
    const workspace = on.find((claim) => claim.path)?.path;
    const holder = agents.find(
      (agent) =>
        agent.path &&
        on.some(
          (claim) =>
            claim.path && (agent.path === claim.path || agent.path?.startsWith(`${claim.path}/`)),
        ),
    );
    // In progress: an agent works in its tree, the tree was used lately, or the landing
    // service has it. A tree nobody uses holds work no one is doing: orphaned.
    const working =
      holder !== undefined || on.some((claim) => !claim.path || (claim.idleMs ?? 0) < WORKING_MS);
    return {
      ...issue,
      state:
        issue.state === "ready" || issue.state === "unscoped"
          ? working
            ? "in_progress"
            : "orphaned"
          : issue.state,
      owner: issue.owner ?? holder?.name ?? null,
      claims: on,
      ...(workspace ? { workspace } : {}),
    };
  });
}
