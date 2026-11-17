import { test } from "node:test";
import assert from "node:assert/strict";
import { apiAddress } from "./address.ts";

const ports = { web: 4310, api: 4311 };
const page = (url: string) => new URL(url);

test("the browser finds the API from where it loaded the page", () => {
  assert.equal(apiAddress(page("http://127.0.0.1:4310/tasks"), ports), "ws://127.0.0.1:4311/rpc");
  assert.equal(
    apiAddress(page("http://localhost:4310/"), ports),
    "ws://localhost:4311/rpc",
    "an SSH tunnel on the same ports",
  );
  assert.equal(
    apiAddress(page("https://desk.example-tailnet.ts.net/agents"), ports),
    "wss://desk.example-tailnet.ts.net/rpc",
    "a proxy routes /rpc on the same address",
  );
  assert.equal(apiAddress(page("http://[::1]:4310/"), ports), "ws://[::1]:4311/rpc");
  assert.equal(
    apiAddress(page("https://example.test/"), ports, "wss://api.example.test/rpc"),
    "wss://api.example.test/rpc",
    "an explicit URL wins",
  );
});
