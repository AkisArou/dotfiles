// nts's streams (docs/plan.md, Streams): their order, as the plan's Streams
// section names their parent issues, and the Tasks tab grouped by them, each
// stream with its progress.

import { filter, taskState, views, type ListView, type Task, type ViewContext } from "signalbox";
import { sections } from "signalbox/blocks";

/** The streams in plan.md's order: the parent issues its Streams section names, first mention first. */
export function streamOrder(context: ViewContext): ReadonlyMap<string, number> {
  const plan = context.snapshot.documents.find((document) => document.id === "docs/plan.md");
  const streams = plan ? (sections(plan.markdown).find((section) => section.title === "Streams")?.markdown ?? "") : "";
  const order = new Map<string, number>();
  for (const [, issue] of streams.matchAll(/#(\d+)/g)) if (issue && !order.has(issue)) order.set(issue, order.size);
  return order;
}

/**
 * The Tasks tab: each stream's tasks under it, in plan.md's order, with its
 * progress (its sub-issues closed, of all of them) and how many are in each
 * state but ready; tasks of no stream, and the local board's chores, last.
 * Filtered on f by state, then nts's label families (.github/labels.json),
 * then by label, board and owner.
 */
export const tasks: ListView<Task> = {
  ...views.tasks,
  items: (context) => {
    const order = streamOrder(context);
    const place = (task: Task) => (task.parent ? (order.get(task.parent.id) ?? order.size) : order.size + 1);
    // A stable sort: in each stream, the tasks in progress stay first.
    return [...views.tasks.items(context)].sort((a, b) => place(a) - place(b));
  },
  group: (task) => task.parent?.title ?? "No stream",
  heading: (group, items, context) => {
    const progress = items[0]?.parent?.progress;
    // How many are in each state but ready, in the states' order: "2 in progress, 3 held".
    const states = new Map<string, { rank: number; count: number }>();
    for (const task of items) {
      const state = taskState(task, context.snapshot.tasks, context.now);
      if (state.label === "ready") continue;
      states.set(state.label, { rank: state.rank, count: (states.get(state.label)?.count ?? 0) + 1 });
    }
    const said = [...states].sort(([, a], [, b]) => a.rank - b.rank).map(([label, { count }]) => `${count} ${label}`);
    return { text: group, count: `${items.length} open${said.length ? `, ${said.join(", ")}` : ""}${progress ? ` · ${progress.done}/${progress.total} done` : ""}` };
  },
  // By state first, then nts's label families, then signalbox's others: label, board and owner.
  filters: [filter.states(), filter.labels("area:"), filter.labels("kind:"), ...(views.tasks.filters ?? []).filter((each) => each.id !== "state")],
};
