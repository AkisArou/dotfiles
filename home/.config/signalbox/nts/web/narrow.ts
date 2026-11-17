// Whether the page is phone-wide: views then show rows instead of wide tables, and
// the shell puts its views in a bottom tab bar, as a native app does.
import { useSyncExternalStore } from "react";

export const NARROW = "(max-width: 760px)";

const subscribe = (change: () => void) => {
  const query = window.matchMedia(NARROW);
  query.addEventListener("change", change);
  return () => query.removeEventListener("change", change);
};

export function useNarrow(): boolean {
  return useSyncExternalStore(
    subscribe,
    () => window.matchMedia(NARROW).matches,
    () => false,
  );
}
