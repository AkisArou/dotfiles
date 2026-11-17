// A details screen (an issue, an agent) on a phone moves as an iOS push: it
// slides in from the right (theme.css), and its back button slides it out before
// it closes. A swipe back is iOS's own gesture, already animated: it closes at once.
import { useCallback, useEffect, useState } from "react";
import type { AnimationEvent } from "react";

const still = () =>
  window.matchMedia("(prefers-reduced-motion: reduce)").matches ||
  !window.matchMedia("(max-width: 760px)").matches;

/** `key` names what the screen shows: a new one starts in place, not leaving. */
export function useSlideBack(close: () => void, key: unknown) {
  const [leaving, setLeaving] = useState(false);
  useEffect(() => setLeaving(false), [key]);
  const back = useCallback(() => (still() ? close() : setLeaving(true)), [close]);
  const onAnimationEnd = useCallback(
    (event: AnimationEvent<HTMLElement>) => {
      if (leaving && event.target === event.currentTarget && event.animationName === "screen-out")
        close();
    },
    [leaving, close],
  );
  return { leaving, back, onAnimationEnd };
}
