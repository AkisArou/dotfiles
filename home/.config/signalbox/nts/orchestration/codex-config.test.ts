import { test } from "node:test";
import assert from "node:assert/strict";
import { codexModelIn } from "./codex-config.ts";
import { profileLabel } from "./models.ts";

test("Codex's configured model is named in the lists, not just 'configured'", () => {
  assert.equal(
    codexModelIn('# comment\nmodel = "gpt-6.1-sol"\n[profiles.fast]\nmodel = "x"\n'),
    "gpt-6.1-sol",
  );
  assert.equal(
    codexModelIn('[profiles.fast]\nmodel = "x"\n'),
    undefined,
    "a profile's model is not the default",
  );
  assert.equal(
    profileLabel({ harness: "codex", effort: "high" }, "gpt-6.1-sol"),
    "Codex gpt-6.1-sol · high",
  );
  assert.equal(
    profileLabel({ harness: "codex", effort: "high" }),
    "Codex (configured model) · high",
  );
});
