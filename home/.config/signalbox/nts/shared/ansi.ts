// A terminal's coloured output (SGR escapes, as tmux's capture-pane -e writes
// them) as styled runs per line, for a transcript a page can render and scroll.
// Other escapes are dropped: a transcript shows text, not cursor movement.

export type Colour = { readonly index: number } | { readonly rgb: string };
export type Style = {
  readonly fg?: Colour;
  readonly bg?: Colour;
  readonly bold?: true;
  readonly dim?: true;
  readonly italic?: true;
  readonly underline?: true;
  readonly inverse?: true;
  readonly strike?: true;
};
export type Run = { readonly text: string; readonly style: Style };

// eslint-disable-next-line no-control-regex
const ESCAPE =
  /\u001b(?:\[([0-9;:]*)([A-Za-z])|\][^\u0007\u001b]*(?:\u0007|\u001b\\)|[()][0-9A-Za-z]|.)/g;

/** Each line of the text as runs of one style; a style carries over from line to line. */
export function parseAnsi(text: string): Run[][] {
  const lines: Run[][] = [];
  let style: Style = {};
  for (const line of text.replace(/\r\n?/g, "\n").replace(/\n$/, "").split("\n")) {
    const runs: Run[] = [];
    let at = 0;
    for (const match of line.matchAll(ESCAPE)) {
      if (match.index > at) runs.push({ text: line.slice(at, match.index), style });
      at = match.index + match[0].length;
      if (match[2] === "m") style = applySgr(style, match[1] ?? "");
    }
    if (at < line.length) runs.push({ text: line.slice(at), style });
    lines.push(runs);
  }
  return lines;
}

function applySgr(previous: Style, parameters: string): Style {
  const codes = (parameters || "0").split(/[;:]/).map((code) => Number(code || 0));
  let style: { -readonly [K in keyof Style]: Style[K] } = { ...previous };
  for (let i = 0; i < codes.length; i++) {
    const code = codes[i]!;
    if (code === 0) style = {};
    else if (code === 1) style.bold = true;
    else if (code === 2) style.dim = true;
    else if (code === 3) style.italic = true;
    else if (code === 4) style.underline = true;
    else if (code === 7) style.inverse = true;
    else if (code === 9) style.strike = true;
    else if (code === 22) (delete style.bold, delete style.dim);
    else if (code === 23) delete style.italic;
    else if (code === 24) delete style.underline;
    else if (code === 27) delete style.inverse;
    else if (code === 29) delete style.strike;
    else if (code >= 30 && code <= 37) style.fg = { index: code - 30 };
    else if (code >= 90 && code <= 97) style.fg = { index: code - 90 + 8 };
    else if (code === 39) delete style.fg;
    else if (code >= 40 && code <= 47) style.bg = { index: code - 40 };
    else if (code >= 100 && code <= 107) style.bg = { index: code - 100 + 8 };
    else if (code === 49) delete style.bg;
    else if (code === 38 || code === 48) {
      const key = code === 38 ? "fg" : "bg";
      if (codes[i + 1] === 5 && codes[i + 2] !== undefined) {
        style[key] = { index: codes[i + 2]! };
        i += 2;
      } else if (codes[i + 1] === 2 && codes[i + 4] !== undefined) {
        const [r, g, b] = [codes[i + 2]!, codes[i + 3]!, codes[i + 4]!];
        style[key] = { rgb: `rgb(${r}, ${g}, ${b})` };
        i += 4;
      }
    }
  }
  return style;
}

/** A 256-colour index above 15 as CSS (the 6×6×6 cube, then the grey ramp). */
export function paletteColour(index: number): string {
  if (index >= 232) {
    const level = 8 + (index - 232) * 10;
    return `rgb(${level}, ${level}, ${level})`;
  }
  const cube = index - 16;
  const step = (value: number) => (value === 0 ? 0 : 55 + value * 40);
  return `rgb(${step(Math.floor(cube / 36))}, ${step(Math.floor(cube / 6) % 6)}, ${step(cube % 6)})`;
}

/**
 * A line as a transcript shows it, at any width: the spaces a program pads it with to
 * its terminal's width go, and when that padding had a background (a prompt's band),
 * the whole line takes that background instead.
 */
export function fitLine(runs: readonly Run[]): { runs: Run[]; background?: Colour } {
  const kept = [...runs];
  let background: Colour | undefined;
  while (kept.length) {
    const last = kept.at(-1)!;
    const trimmed = last.text.replace(/\s+$/, "");
    if (trimmed === last.text) break;
    if (!trimmed) {
      background ??= last.style.inverse ? last.style.fg : last.style.bg;
      kept.pop();
      continue;
    }
    if (last.style.bg && !background) background = last.style.bg;
    kept[kept.length - 1] = { ...last, text: trimmed };
    break;
  }
  return background ? { runs: kept, background } : { runs: kept };
}
