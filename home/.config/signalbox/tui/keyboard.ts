import type { KeyEvent } from "@opentui/core";

type Motion =
  | { kind: "move"; direction: 1 | -1; distance: "line" | "half" | "page" }
  | { kind: "edge"; last: boolean }
  | { kind: "tab"; direction: 1 | -1 }
  | { kind: "prefix" };

/** Navigation only. Editors, native terminals and external tools own their own input. */
export function navigationKey(
  key: Pick<KeyEvent, "name" | "ctrl" | "meta" | "option" | "shift">,
  afterG: boolean,
): { afterG: boolean; motion?: Motion } {
  const plain = !key.ctrl && !key.meta && !key.option;
  const name = key.shift && key.name.length === 1 ? key.name.toUpperCase() : key.name;
  let motion: Motion | undefined;
  if (plain) {
    if (afterG && name === "g") motion = { kind: "edge", last: false };
    else if (afterG && (name === "t" || name === "T"))
      motion = { kind: "tab", direction: name === "t" ? 1 : -1 };
    else if (name === "g") return { afterG: true, motion: { kind: "prefix" } };
    else if (name === "G" || name === "end") motion = { kind: "edge", last: true };
    else if (name === "home") motion = { kind: "edge", last: false };
    else if (name === "tab") motion = { kind: "tab", direction: key.shift ? -1 : 1 };
    else if (name === "j" || name === "down")
      motion = { kind: "move", direction: 1, distance: "line" };
    else if (name === "k" || name === "up")
      motion = { kind: "move", direction: -1, distance: "line" };
    else if (name === "pagedown" || name === "pageup")
      motion = { kind: "move", direction: name === "pagedown" ? 1 : -1, distance: "page" };
  } else if (key.ctrl && !key.meta && !key.option && !key.shift) {
    if (name === "d" || name === "u")
      motion = { kind: "move", direction: name === "d" ? 1 : -1, distance: "half" };
    else if (name === "f" || name === "b")
      motion = { kind: "move", direction: name === "f" ? 1 : -1, distance: "page" };
  }
  // An unrelated key cancels the prefix and still performs its ordinary action.
  return { afterG: false, motion };
}
