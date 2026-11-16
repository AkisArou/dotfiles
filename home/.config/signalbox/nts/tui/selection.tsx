/** @jsxImportSource @opentui/react */
import { createContext, useContext, useEffect, useState } from "react";
import type { ReactNode } from "react";
import { useRenderer } from "@opentui/react";
import { CliRenderEvents, RGBA, TextAttributes } from "@opentui/core";
import type { TerminalColors } from "@opentui/core";
import { colors as c } from "./theme.ts";

// Terminal defaults remain usable even when the emulator cannot answer OSC queries.
const fallback = { background: c.background, text: c.text, attributes: TextAttributes.INVERSE };
const Selection = createContext(fallback);
export const useSelection = () => useContext(Selection);

export function TerminalTheme({ children }: { children: ReactNode }) {
  const renderer = useRenderer();
  const [selection, setSelection] = useState(fallback);
  useEffect(() => {
    let active = true;
    const apply = (palette: TerminalColors) => {
      if (!active) return;
      if (palette.highlightBackground) {
        setSelection({
          background: RGBA.fromHex(palette.highlightBackground),
          text: palette.highlightForeground ? RGBA.fromHex(palette.highlightForeground) : c.text,
          attributes: 0,
        });
      } else if (palette.defaultBackground && palette.defaultForeground) {
        // A subtle surface derived from the actual terminal, rather than v1's fixed gray.
        const bg = RGBA.fromHex(palette.defaultBackground),
          fg = RGBA.fromHex(palette.defaultForeground);
        setSelection({
          background: RGBA.fromValues(
            bg.r + (fg.r - bg.r) * 0.16,
            bg.g + (fg.g - bg.g) * 0.16,
            bg.b + (fg.b - bg.b) * 0.16,
          ),
          text: c.text,
          attributes: 0,
        });
      }
    };
    renderer.on(CliRenderEvents.PALETTE, apply);
    // Do not hold up the first frame, or require color-query support.
    void renderer.getPalette({ size: 16, timeout: 300 }).then(apply, () => {});
    return () => {
      active = false;
      renderer.off(CliRenderEvents.PALETTE, apply);
    };
  }, [renderer]);
  return <Selection.Provider value={selection}>{children}</Selection.Provider>;
}
