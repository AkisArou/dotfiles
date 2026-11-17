// An issue (?issue=n) or an agent (?screen=id) opened over a view is a step in
// history: opening one pushes it, and closing it goes back, so the phone's back
// swipe (and the browser's Back) returns to the view it was opened from.
import { useRef } from "react";
import { useNavigate, useRouter } from "@tanstack/react-router";

type Overlay = { issue?: number; screen?: string };

export function useOverlay() {
  const navigate = useNavigate();
  const router = useRouter();
  // The step a close already went back from: a second close for it (the drawer's own
  // and ours can both fire) must not go back again before history has moved.
  const closed = useRef<string>(undefined);
  return {
    open(change: Overlay) {
      void navigate({
        to: ".",
        search: (previous: Record<string, unknown>) => ({ ...previous, ...change }),
        // Marks the step as one this page pushed, so closing may go back to it.
        state: (previous) => ({ ...previous, overlay: true }) as typeof previous,
      });
    },
    close(key: keyof Overlay) {
      const location = router.state.location;
      if (closed.current === location.state.key) return;
      closed.current = location.state.key;
      const state = location.state as { overlay?: boolean };
      if (state.overlay) router.history.back();
      // Opened from a link (nothing of ours to go back to): drop it in place.
      else
        void navigate({
          to: ".",
          search: (previous: Record<string, unknown>) => ({ ...previous, [key]: undefined }),
          replace: true,
        });
    },
  };
}
