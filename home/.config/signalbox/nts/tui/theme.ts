import { RGBA } from "@opentui/core";

// USER STYLING: retain terminal defaults and ANSI slots rather than fixed RGB values.
// The terminal owns these colors, including light/dark themes and transparency.
export const colors = {
  background: RGBA.defaultBackground(),
  text: RGBA.defaultForeground(),
  border: RGBA.fromIndex(8), // Bright black: the terminal's secondary neutral.
  muted: RGBA.fromIndex(8),
  accent: RGBA.fromIndex(6), // Cyan.
  success: RGBA.fromIndex(2), // Green.
  warning: RGBA.fromIndex(3), // Yellow.
  danger: RGBA.fromIndex(1), // Red.
  info: RGBA.fromIndex(4), // Blue.
};
