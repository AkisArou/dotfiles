// NTS's observed work, not a Signalbox assignment or ownership policy.
import type { Agent, Issue } from "./model.ts";
import type { WorkClaim } from "../nts/model.ts";

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
    return {
      ...issue,
      state: issue.state === "ready" || issue.state === "unscoped" ? "in_progress" : issue.state,
      owner: issue.owner ?? holder?.name ?? null,
      claims: on,
      ...(workspace ? { workspace } : {}),
    };
  });
}
