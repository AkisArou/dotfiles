// The wake-up feed against stand-in sources: what each source held before the
// coordinator first saw it ready is history; what arrives after is news, once.
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { capability, Cell, defineConfig, provide, startExtensions } from "signalbox/core";
import type { Readable } from "signalbox/core";
import type { Assignments } from "signalbox/ext/assignments";
import type { Mailbox } from "signalbox/ext/orchestration";
import { sqliteWorkflowState } from "signalbox/ext/integrations/sqlite";
import type { LandingStatus } from "../status/landing.ts";
import type { FullCheckStatus } from "../status/full-check.ts";
import type { Questions } from "./needs.ts";
import type { NeedsView } from "../shared/model.ts";
import { sourceOf, wakeupFeed } from "./wakeups.ts";

const landed = (id: string) =>
  ({
    service: { running: true, state: "idle", since: "", message: null },
    checking: null,
    waiting: [],
    held: [],
    day: { landed: 0, refused: 0, held: 0, withdrawn: 0, median_wall_s: null, max_wall_s: null },
    recent: [
      {
        id,
        sha: id.padEnd(40, "0"),
        state: "refused",
        at: "",
        message: "red",
        branch: null,
        issue: 7,
        run: null,
      },
    ],
    main: { commit: null, pushed: null },
  }) as unknown as LandingStatus;

test("wake-up sources: landing, culprit and decided IDs name their source", () => {
  assert.equal(sourceOf("landing/a/refused"), "landing");
  assert.equal(sourceOf("culprit/c/core/open"), "fullCheck");
  assert.equal(sourceOf("decided/q1"), "decisions");
  assert.equal(sourceOf("design/a1/x"), "assignments");
});

test("a source's history before it first reads ready is not news; later events are, once", async (t) => {
  const directory = await mkdtemp(join(tmpdir(), "nts-wakeups-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  const sent: string[] = [];
  const key = <T>(id: string) => capability<T>(id);
  const work = key<Assignments>("work");
  const landing = key<Readable<LandingStatus>>("landing");
  const fullCheck = key<Readable<FullCheckStatus>>("full-check");
  const decisions = key<Questions>("decisions");
  const needs = key<Readable<NeedsView>>("needs");
  const mailbox = key<Mailbox>("mailbox");
  const landingCell = new Cell<LandingStatus>(); // loading, as at a host's start
  const state = sqliteWorkflowState({ path: join(directory, "state.sqlite") });
  const app = await startExtensions(
    defineConfig({
      id: "wakeups",
      root: directory,
      gate: { accept: () => false },
      extensions: [
        state.extension,
        {
          id: "stand-ins",
          provides: [
            provide(work, { assignments: new Cell([]) } as unknown as Assignments),
            provide(landing, landingCell),
            provide(fullCheck, new Cell<FullCheckStatus>()),
            provide(decisions, { requests: new Cell([]) } as unknown as Questions),
            provide(needs, new Cell<NeedsView>({ questions: [], unasked: [], answered: [] })),
            provide(mailbox, {
              send: async ({ id }: { id: string }) => void sent.push(id),
            } as unknown as Mailbox),
          ],
        },
        wakeupFeed({ state: state.state, mailbox, work, landing, fullCheck, decisions, needs }),
      ],
    }),
  );
  t.after(() => app.stop());
  const settle = () => new Promise((resolve) => setTimeout(resolve, 5_500));
  await settle();
  landingCell.set(landed("old")); // the landing history arrives after the first observation
  await settle();
  assert.deepEqual(sent, [], "what the landing source held when first ready is history");
  landingCell.set(landed("new"));
  await settle();
  assert.deepEqual(sent, ["landing/new/refused"]);
  await settle();
  assert.deepEqual(sent, ["landing/new/refused"], "delivered once");
});
