// On a phone, the views sit in a bottom tab bar, as in a native app: the four
// you use most, and More, a sheet with the rest, the appearance and the status
// line the desktop shows at its foot.
import type { ReactNode, SVGProps } from "react";
import { useLocation, useNavigate } from "@tanstack/react-router";
import { BottomSheet } from "@astryxdesign/core/BottomSheet";
import { Button } from "@astryxdesign/core/Button";
import { List, ListItem } from "@astryxdesign/core/List";
import { Text } from "@astryxdesign/core/Text";
import type { Tab as View } from "../shared/model.ts";
import { tabs } from "../shared/model.ts";
import { viewPath } from "./project.ts";

const PRIMARY: readonly View[] = ["Now", "Needs you", "Tasks", "Agents"];

/** Stroke icons in the text colour, 24 px, for the tab bar. */
function Glyph({ children, ...props }: SVGProps<SVGSVGElement> & { children: ReactNode }) {
  return (
    <svg
      viewBox="0 0 24 24"
      width="24"
      height="24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.8"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      {...props}
    >
      {children}
    </svg>
  );
}
const ICONS: Record<string, ReactNode> = {
  Now: (
    <Glyph>
      <polyline points="3 12 7 12 10 5 14 19 17 12 21 12" />
    </Glyph>
  ),
  "Needs you": (
    <Glyph>
      <path d="M6 9a6 6 0 0 1 12 0c0 6 2.5 7.5 2.5 7.5h-17S6 15 6 9" />
      <path d="M10 20a2.2 2.2 0 0 0 4 0" />
    </Glyph>
  ),
  Tasks: (
    <Glyph>
      <path d="M4 6.5l1.5 1.5L8 5.5M4 12.5l1.5 1.5L8 11.5M4 18.5l1.5 1.5L8 17.5" />
      <path d="M11 7h9M11 13h9M11 19h9" />
    </Glyph>
  ),
  Agents: (
    <Glyph>
      <rect x="3" y="4.5" width="18" height="15" rx="2.5" />
      <path d="M7 10l3 2.5L7 15M12.5 15H17" />
    </Glyph>
  ),
  More: (
    <Glyph>
      <circle cx="5.5" cy="12" r="1.2" />
      <circle cx="12" cy="12" r="1.2" />
      <circle cx="18.5" cy="12" r="1.2" />
    </Glyph>
  ),
};

export function TabBar({
  view,
  counts,
  more,
  setMore,
  status,
  appearance,
  notifications,
  onReselect,
}: {
  view: View;
  counts: Partial<Record<View, number>>;
  more: boolean;
  setMore(open: boolean): void;
  /** The status line the desktop shows at its foot. */
  status: string;
  appearance: ReactNode;
  /** This device's notifications switch. */
  notifications: ReactNode;
  /** The tab you are on, tapped again at its own page: scroll that page to its top. */
  onReselect(): void;
}) {
  const navigate = useNavigate();
  const { pathname } = useLocation();
  // As an iOS tab bar: tapping the tab you are on goes back to its own page from
  // deeper in it (Agents › Settings), and from its own page scrolls to the top.
  const go = (to: View) => {
    setMore(false);
    if (to === view && pathname === viewPath[to]) return onReselect();
    void navigate({ to: viewPath[to] });
  };
  const rest = tabs.filter((name) => !PRIMARY.includes(name));
  const item = (name: string, active: boolean, onPress: () => void, count?: number) => (
    <button
      key={name}
      type="button"
      className={`tab-bar-item${active ? " active" : ""}`}
      aria-current={active ? "page" : undefined}
      onClick={onPress}
    >
      <span className="tab-bar-icon">
        {ICONS[name]}
        {count ? <span className="tab-bar-badge">{count > 99 ? "99+" : count}</span> : null}
      </span>
      <span className="tab-bar-label">{name}</span>
    </button>
  );
  return (
    <>
      <nav className="tab-bar" aria-label="Views">
        {PRIMARY.map((name) => item(name, view === name, () => go(name), counts[name]))}
        {item("More", rest.includes(view), () => setMore(true))}
      </nav>
      <BottomSheet label="More views" isOpen={more} onOpenChange={setMore} height="auto">
        <List density="compact" hasDividers edge="padded">
          {rest.map((name) => (
            <ListItem key={name} label={name} isSelected={view === name} onClick={() => go(name)} />
          ))}
        </List>
        <div className="sheet-foot">
          {notifications}
          {appearance}
          <Text type="supporting" color="secondary">
            {status}
          </Text>
        </div>
      </BottomSheet>
    </>
  );
}

/** The appearance switch, the same control in the header and the More sheet. */
export function AppearanceButton({
  mode,
  next,
  onSwitch,
}: {
  mode: string;
  next: string;
  onSwitch(): void;
}) {
  return (
    <Button
      label={`Appearance: ${mode}; switch to ${next}`}
      variant="ghost"
      size="sm"
      onClick={onSwitch}
    >
      {mode}
    </Button>
  );
}
