import { test } from "node:test";
import assert from "node:assert/strict";
import { tailnetOrigin } from "./tailnet.ts";

test("the tailnet origin comes from Tailscale's own status, not from config", () => {
  assert.equal(
    tailnetOrigin({ Self: { DNSName: "desk.example-tailnet.ts.net." } }),
    "https://desk.example-tailnet.ts.net",
  );
  assert.equal(tailnetOrigin({ Self: {} }), undefined);
  assert.equal(tailnetOrigin(null), undefined);
});
