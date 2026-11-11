// nts's streams (docs/plan.md, Streams): their order, as the plan's Streams
// section names their parent issues, and the Tasks tab grouped by them. A
// stream holds groups (`kind:group`, a foundation or theme) and a group holds
// tasks (docs/workflow.md, Tasks); `task` marks a leaf ready to take, so a leaf
// without it is listed as unscoped.

import { filter, taskState, views, type ListView, type Task, type ViewContext } from "signalbox";
import { sections } from "signalbox/blocks";

const GROUP = "kind:group";
const READY = "task";
/** A leaf that is not ready and not set aside by a state label: its card still has to be scoped. */
const UNSCOPED = "unscoped";
/** A group listed as a row: its parts are still its card's checklist, not task issues. */
const GROUP_STATE = "group";

/** The streams in plan.md's order: the parent issues its Streams section names, first mention first. */
export function streamOrder(context: ViewContext): ReadonlyMap<string, number> {
  const plan = context.snapshot.documents.find((document) => document.id === "docs/plan.md");
  const streams = plan ? (sections(plan.markdown).find((section) => section.title === "Streams")?.markdown ?? "") : "";
  const order = new Map<string, number>();
  for (const [, issue] of streams.matchAll(/#(\d+)/g)) if (issue && !order.has(issue)) order.set(issue, order.size);
  return order;
}

export const isGroup = (task: Task): boolean => task.labels?.includes(GROUP) ?? false;

type Ref = { readonly id: string; readonly title: string };
export type Place = { readonly stream?: Ref; readonly group?: Ref & { readonly progress?: { readonly done: number; readonly total: number } | undefined } };

/** Where a leaf sits: its group when its parent is one, and the stream above that; a leaf right under a stream has no group. */
export function placeOf(task: Task, byId: ReadonlyMap<string, Task>): Place {
  const parent = task.parent;
  if (!parent) return {};
  const group = byId.get(parent.id);
  if (group && isGroup(group)) return { group: parent, ...(group.parent ? { stream: { id: group.parent.id, title: group.parent.title } } : {}) };
  return { stream: { id: parent.id, title: parent.title } };
}

/** The open leaves (groups are listed only as headings), each unscoped one marked so: no agent takes it until its card is ready. */
export function leaves(tasks: readonly Task[]): Task[] {
  return tasks
    .filter((task) => !isGroup(task))
    .map((task) => (task.labels?.includes(READY) || task.state !== undefined || task.status === "in_progress" ? task : { ...task, status: "other", state: UNSCOPED }));
}

const byIdOf = (context: ViewContext) => new Map(context.snapshot.tasks.map((task) => [task.id, task] as const));

/**
 * The Tasks tab: every open leaf under its stream and group, streams in
 * plan.md's order and groups by title, each heading with its group's progress
 * (its sub-issues closed, of all of them) and how many are in each state but
 * ready; leaves of no stream, and the local board's chores, last. Filtered on
 * f by state, then nts's label families (.github/labels.json), then by label,
 * board and owner.
 */
export const tasks: ListView<Task> = {
  ...views.tasks,
  items: (context) => {
    const order = streamOrder(context);
    const byId = byIdOf(context);
    const key = (task: Task) => {
      const place = placeOf(task, byId);
      return { stream: place.stream ? (order.get(place.stream.id) ?? order.size) : order.size + 1, group: place.group?.title ?? "" };
    };
    const open = views.tasks.items(context);
    const listed = leaves(open);
    // A group whose parts are not yet task issues is listed itself, under its stream, so no work is out of sight.
    const parents = new Set(listed.flatMap((task) => (task.parent ? [task.parent.id] : [])));
    const unsplit = open.filter((task) => isGroup(task) && !parents.has(task.id)).map((task): Task => ({ ...task, status: "other", state: GROUP_STATE }));
    // A stable sort: in each group, the tasks in progress stay first.
    return [...listed, ...unsplit].sort((a, b) => {
      const ka = key(a);
      const kb = key(b);
      return ka.stream - kb.stream || ka.group.localeCompare(kb.group);
    });
  },
  group: (task, context) => {
    const place = placeOf(task, byIdOf(context));
    if (!place.stream) return "No stream";
    return place.group ? `${place.stream.title} › ${place.group.title}` : place.stream.title;
  },
  heading: (group, items, context) => {
    const progress = items[0]?.parent?.progress;
    // How many are in each state but ready, in the states' order: "2 in progress, 3 unscoped".
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
