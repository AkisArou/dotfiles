// nts's issues live on GitHub: the tracker the kit's Needs you writes to labels an
// issue state:needs-user while the owner's answer is awaited, then posts the
// answer as a comment and moves the issue by its state label.
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import type { IssueState } from "@signalbox/kit/shared/protocol";
import type { IssueTracker } from "@signalbox/kit/orchestration/needs";

/** Runs gh against the project's repository; returns stdout. */
export type GitHub = (args: readonly string[]) => Promise<string>;
export function gitHub(repo: string): GitHub {
  const run = promisify(execFile);
  return async (args) =>
    (await run("gh", [...args, "--repo", repo], { timeout: 60_000, maxBuffer: 8 << 20 })).stdout;
}

const WAITING = "state:needs-user";
const LABEL: Record<IssueState, string | null> = {
  ready: null,
  parked: "state:parked",
  held: "state:held",
};

export function gitHubTracker(gh: GitHub): IssueTracker {
  const labels = async (issue: number) =>
    (
      JSON.parse(await gh(["issue", "view", String(issue), "--json", "labels"])) as {
        labels: { name: string }[];
      }
    ).labels.map((label) => label.name);
  return {
    async waiting(issue) {
      if (!(await labels(issue)).includes(WAITING))
        await gh(["issue", "edit", String(issue), "--add-label", WAITING]);
    },
    async answered(issue, { comment, then }) {
      await gh(["issue", "comment", String(issue), "--body", comment]);
      const have = await labels(issue);
      const label = LABEL[then];
      const edit = [
        ...(have.includes(WAITING) ? ["--remove-label", WAITING] : []),
        ...(label && !have.includes(label) ? ["--add-label", label] : []),
      ];
      if (edit.length) await gh(["issue", "edit", String(issue), ...edit]);
    },
  };
}
