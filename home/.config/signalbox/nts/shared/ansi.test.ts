import { test } from "node:test";
import assert from "node:assert/strict";
import { fitLine, paletteColour, parseAnsi } from "./ansi.ts";

const ESC = "\u001b";

test("coloured output becomes styled runs per line; other escapes are dropped", () => {
  const lines = parseAnsi(
    `plain ${ESC}[1;31mbold red${ESC}[0m done\n${ESC}[38;5;208morange${ESC}[39m ${ESC}[48;2;10;20;30mrgb${ESC}[0m${ESC}]0;title\u0007\n`,
  );
  assert.equal(lines.length, 2, "the trailing newline ends the last line");
  assert.deepEqual(lines[0], [
    { text: "plain ", style: {} },
    { text: "bold red", style: { bold: true, fg: { index: 1 } } },
    { text: " done", style: {} },
  ]);
  assert.deepEqual(lines[1], [
    { text: "orange", style: { fg: { index: 208 } } },
    { text: " ", style: {} },
    { text: "rgb", style: { bg: { rgb: "rgb(10, 20, 30)" } } },
  ]);
});

test("a style carries over to the next line until it is reset", () => {
  const lines = parseAnsi(`${ESC}[32mgreen\nstill green${ESC}[m\nplain`);
  assert.deepEqual(lines[1], [{ text: "still green", style: { fg: { index: 2 } } }]);
  assert.deepEqual(lines[2], [{ text: "plain", style: {} }]);
});

test("the 256-colour cube and grey ramp", () => {
  assert.equal(paletteColour(16), "rgb(0, 0, 0)");
  assert.equal(paletteColour(231), "rgb(255, 255, 255)");
  assert.equal(paletteColour(232), "rgb(8, 8, 8)");
});

test("a line padded to the terminal's width fits any width; padded with a background, it becomes a band", () => {
  const [prompt] = parseAnsi(`${ESC}[48;5;237m> fix the bug${" ".repeat(120)}${ESC}[0m`);
  const fitted = fitLine(prompt!);
  assert.deepEqual(fitted.runs, [{ text: "> fix the bug", style: { bg: { index: 237 } } }]);
  assert.deepEqual(fitted.background, { index: 237 }, "the band keeps the prompt's background");
  const [plain] = parseAnsi(`output   `);
  assert.deepEqual(fitLine(plain!), { runs: [{ text: "output", style: {} }] });
  const [band] = parseAnsi(`${ESC}[48;5;237m${" ".repeat(40)}${ESC}[0m`);
  assert.deepEqual(fitLine(band!), { runs: [], background: { index: 237 } });
});
