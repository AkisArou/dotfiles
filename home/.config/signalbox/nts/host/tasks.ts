// NTS's GitHub conventions, ported from its v1 configuration. Reads use your gh login.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { z } from "zod";
import type { Issue } from "../shared/model.ts";

const run = promisify(execFile);
const listedIssue = z.object({
  number: z.number().int().positive(),
  title: z.string(),
  body: z.string().default(""),
  url: z.url(),
  labels: z.array(z.object({ name: z.string() })),
  assignees: z.array(z.object({ login: z.string() })),
  parent: z.object({ number: z.number(), title: z.string() }).nullish(),
  subIssuesSummary: z.object({ total: z.number(), completed: z.number() }).nullish(),
});
export type ListedIssue = z.infer<typeof listedIssue>;

export async function readIssues(
  repo: string,
  limit: number,
  signal: AbortSignal,
): Promise<ListedIssue[]> {
  if (!/^[\w.-]+\/[\w.-]+$/.test(repo)) throw new Error("NTS_GITHUB_REPO must be owner/name");
  const { stdout } = await run(
    "gh",
    [
      "issue",
      "list",
      "--repo",
      repo,
      "--state",
      "open",
      "--limit",
      String(limit + 1),
      "--json",
      "number,title,body,url,labels,assignees,parent,subIssuesSummary",
    ],
    { signal, timeout: 60_000, maxBuffer: 32 * 1024 * 1024 },
  );
  const listed = z.array(listedIssue).parse(JSON.parse(stdout));
  if (listed.length > limit)
    throw new Error(
      `More than ${limit} open NTS issues; increase issueLimit in playground/config.ts`,
    );
  return listed;
}

export function projectIssues(listed: readonly ListedIssue[], plan: string): Issue[] {
  const byNumber = new Map(listed.map((issue) => [issue.number, issue]));
  const labelsOf = (issue: ListedIssue) => issue.labels.map((label) => label.name);
  const order = [...plan.matchAll(/#(\d+)\b/g)].map((match) => Number(match[1]));
  const rank = (number: number) => {
    const index = order.indexOf(number);
    return index < 0 ? Number.MAX_SAFE_INTEGER : index;
  };
  const projected = listed
    .filter((issue) => {
      const labels = labelsOf(issue);
      if (labels.includes("kind:parent") || labels.includes("signalbox:state")) return false;
      // Groups with no open child must remain visible, even before tasks are scoped.
      return (
        !labels.includes("kind:group") ||
        !listed.some((child) => child.parent?.number === issue.number)
      );
    })
    .map((issue) => {
      const labels = labelsOf(issue);
      const ancestors: ListedIssue[] = [];
      const seen = new Set([issue.number]);
      let parent = issue.parent;
      while (parent && !seen.has(parent.number)) {
        seen.add(parent.number);
        const ancestor = byNumber.get(parent.number);
        if (!ancestor) break;
        ancestors.push(ancestor);
        parent = ancestor.parent;
      }
      const group = ancestors.find((ancestor) => labelsOf(ancestor).includes("kind:group"));
      const stream = ancestors.find((ancestor) => labelsOf(ancestor).includes("kind:parent"));
      const state = labels.includes("state:needs-user")
        ? "needs-user"
        : labels.includes("in-progress")
          ? "in_progress"
          : labels.includes("state:held") || labels.includes("hold:all")
            ? "held"
            : labels.includes("state:parked")
              ? "parked"
              : labels.includes("kind:group")
                ? "group"
                : labels.includes("task")
                  ? "ready"
                  : "unscoped";
      const blockers = /^\s*blocked by:\s*(.+)$/im.exec(issue.body)?.[1] ?? "";
      const owner =
        labels.find((label) => label.startsWith("agent:"))?.slice(6) ??
        issue.assignees.map((assignee) => assignee.login).join(", ");
      return {
        id: String(issue.number),
        number: issue.number,
        title: issue.title,
        body: issue.body,
        url: issue.url,
        labels,
        state,
        area: labels.find((label) => label.startsWith("area:"))?.slice(5) ?? "unassigned",
        stream: stream?.title ?? issue.parent?.title ?? "Unsorted",
        group: group?.title ?? (labels.includes("kind:group") ? issue.title : ""),
        assignees: issue.assignees.map((assignee) => assignee.login),
        owner: owner || null,
        blockedBy: [...blockers.matchAll(/#(\d+)/g)].map((match) => Number(match[1])),
        parent: issue.parent ? { ...issue.parent } : null,
        progress: issue.subIssuesSummary ? { ...issue.subIssuesSummary } : null,
        streamRank: stream
          ? rank(stream.number)
          : issue.parent
            ? rank(issue.parent.number)
            : Number.MAX_SAFE_INTEGER,
      };
    });
  return projected
    .sort((a, b) => a.streamRank - b.streamRank || a.group.localeCompare(b.group))
    .map(({ streamRank: _, ...issue }) => issue);
}
