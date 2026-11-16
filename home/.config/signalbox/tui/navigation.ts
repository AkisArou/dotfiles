import { tabKey, tabs } from "../shared/model.ts";
import type { Tab } from "../shared/model.ts";

/** Keep every view visible at narrow widths; Tab reaches views beyond the number keys. */
export function tabLabels(current: Tab, width: number, counts: (tab: Tab) => number | undefined) {
  const full = tabs.map(
    (tab, index) =>
      ` ${tabKey(index)} ${tab}${counts(tab) === undefined ? "" : ` ${counts(tab)}`} `,
  );
  if (full.reduce((sum, label) => sum + label.length + 1, 0) <= width) return full;
  const named = tabs.map((tab, index) => ` ${tabKey(index)} ${tab} `);
  if (named.reduce((sum, label) => sum + label.length + 1, 0) <= width) return named;
  return tabs.map((tab, index) =>
    tab === current
      ? ` ${tabKey(index)} ${tab} `
      : ` ${
          tabKey(index) ||
          tab
            .split(" ")
            .map((word) => word.slice(0, 2))
            .join("")
        } `,
  );
}
