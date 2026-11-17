// Notifications on the owner's devices (Web Push) when something new waits on
// them: an open question, or an issue nobody has asked about yet. The host signs
// with its own key pair (VAPID), kept in its state folder. Each device's push
// address lives in the workflow store, and one its push service has forgotten is
// dropped. The push service carries the message encrypted; it cannot read it.
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import webpush from "web-push";
import { z } from "zod";
import { capability, Cell, provide } from "signalbox/core";
import type { Capability, Extension, Readable } from "signalbox/core";
import { hash, toJson } from "signalbox/core/utils";
import type { WorkflowStore } from "signalbox/ext/assignments";
import { exposeOperation, exposeSource } from "signalbox/host";
import * as refs from "../shared/model.ts";
import type { Issue, NeedsView, PushDevice } from "../shared/model.ts";

const NAMESPACE = "nts/push";
/** Who runs this push sender, as push services ask (a contact is required, not checked). */
const SUBJECT = "mailto:signalbox@example.com";

/** One notification: what waits, under a key that is sent once while it waits. */
export type Notice = { id: string; title: string; body: string; url: string };
/** What a device's service worker receives. */
export type PushPayload = Notice & { count: number };

/** What waits on the owner, one notice each. A question asked again is news again. */
export function notices(view: NeedsView): Notice[] {
  const line = (text: string) => text.replace(/\s+/g, " ").trim().slice(0, 240);
  return [
    ...view.questions.map((question) => ({
      id: `question/${question.id}/${question.revision}`,
      title: question.issue ? `Needs you · #${question.issue.number}` : "Needs you",
      body: line(question.question),
      url: "/needs-you",
    })),
    ...view.unasked.map((issue) => ({
      id: `issue/${issue.number}`,
      title: `Needs you · #${issue.number}`,
      body: line(`${issue.title}: it waits on your word`),
      url: "/needs-you",
    })),
  ];
}

/**
 * What to send now: the notices not sent before, and what to remember as sent. The first
 * reading ever sends nothing (what already waits is not news), and an item that stopped
 * waiting is forgotten, so it is news again if it comes back.
 */
export function fresh(
  current: readonly Notice[],
  sent: readonly string[] | undefined,
): { send: Notice[]; sent: string[] } {
  const ids = current.map((notice) => notice.id);
  if (sent === undefined) return { send: [], sent: ids };
  return { send: current.filter((notice) => !sent.includes(notice.id)), sent: ids };
}

type Keys = { publicKey: string; privateKey: string };
type Device = PushDevice & { addedAt: number };
/** Sends one payload to one device; throws with statusCode when its push service refuses. */
export type Sender = (device: PushDevice, payload: PushPayload, keys: Keys) => Promise<void>;

const webPushSender: Sender = async (device, payload, keys) => {
  await webpush.sendNotification(device, JSON.stringify(payload), {
    vapidDetails: { subject: SUBJECT, publicKey: keys.publicKey, privateKey: keys.privateKey },
    TTL: 24 * 60 * 60,
    urgency: "high",
    // A newer message for the same item replaces one not yet delivered.
    topic: hash(payload.id).slice(0, 32),
  });
};

/** The host's key pair, made once and kept private in its state folder. */
async function loadKeys(path: string): Promise<Keys> {
  const read = async () => JSON.parse(await readFile(path, "utf8")) as Keys;
  try {
    return await read();
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const keys = webpush.generateVAPIDKeys();
  try {
    await writeFile(path, JSON.stringify(keys), { mode: 0o600, flag: "wx" });
    return keys;
  } catch (error) {
    // Another start wrote it first: theirs is the key pair.
    if ((error as NodeJS.ErrnoException).code === "EEXIST") return read();
    throw error;
  }
}

export interface PushService {
  subscribe(device: PushDevice): Promise<void>;
  unsubscribe(endpoint: string): Promise<void>;
  test(endpoint: string): Promise<void>;
}
/** The push service, for the operations (and tests) that subscribe and test devices. */
export const pushService = capability<PushService>("nts/push");
const deviceKey = (endpoint: string) => `device/${hash(endpoint)}`;
/** A push service refusing a device it no longer knows: forget the device. */
const gone = (error: unknown) =>
  [404, 410].includes((error as { statusCode?: number }).statusCode ?? 0);

export function pushNotifications(options: {
  readonly state: Capability<WorkflowStore>;
  readonly needs: Capability<Readable<NeedsView>>;
  /** The issues the needs view is made from: it is complete only once they are read. */
  readonly issues: Capability<Readable<Issue[]>>;
  /** The host's state folder, where its key pair is kept. */
  readonly directory: string;
  readonly send?: Sender;
}): Extension[] {
  const send = options.send ?? webPushSender;
  const publicKey = new Cell<{ publicKey: string }>();
  let started: PushService | undefined;
  const lazy: PushService = {
    subscribe: (device) => started!.subscribe(device),
    unsubscribe: (endpoint) => started!.unsubscribe(endpoint),
    test: (endpoint) => started!.test(endpoint),
  };
  return [
    {
      id: "nts-push",
      requires: [options.state, options.needs, options.issues],
      provides: [provide(pushService, lazy)],
      async start(context) {
        const store = context.get(options.state);
        const needs = context.get(options.needs);
        const issues = context.get(options.issues);
        await mkdir(options.directory, { recursive: true, mode: 0o700 });
        const keys = await loadKeys(join(options.directory, "push-keys.json"));
        publicKey.set({ publicKey: keys.publicKey });
        const devices = () =>
          store.transaction(NAMESPACE, (tx) =>
            tx.entries("device/").map((entry) => entry.value as unknown as Device),
          );
        const deliver = async (device: Device, payload: PushPayload) => {
          try {
            await send(device, payload, keys);
          } catch (error) {
            if (!gone(error)) throw error;
            await store.transaction(NAMESPACE, (tx) => tx.delete(deviceKey(device.endpoint)));
          }
        };
        const count = () => {
          const reading = needs.get();
          return reading.status === "ready" ? notices(reading.value).length : 0;
        };
        started = {
          async subscribe(device) {
            const value: Device = { ...device, addedAt: Date.now() };
            await store.transaction(NAMESPACE, (tx) =>
              tx.set(deviceKey(device.endpoint), toJson(value)),
            );
          },
          async unsubscribe(endpoint) {
            await store.transaction(NAMESPACE, (tx) => tx.delete(deviceKey(endpoint)));
          },
          async test(endpoint) {
            const device = (await devices()).find((each) => each.endpoint === endpoint);
            if (!device) throw new Error("This device is not subscribed");
            await deliver(device, {
              id: "test",
              title: "Notifications are on",
              body: "nts will tell you here when something needs you.",
              url: "/needs-you",
              count: count(),
            });
          },
        };
        // One delivery at a time, in order; a failure is logged and the next change retries.
        let chain = Promise.resolve();
        const check = () => {
          chain = chain
            .then(async () => {
              const reading = needs.get();
              if (reading.status !== "ready" || issues.get().status !== "ready") return;
              const current = notices(reading.value);
              const sent = (await store.transaction(NAMESPACE, (tx) => tx.get("sent"))) as
                | string[]
                | undefined;
              const next = fresh(current, sent);
              const targets = next.send.length ? await devices() : [];
              for (const notice of next.send)
                for (const device of targets)
                  await deliver(device, { ...notice, count: current.length });
              await store.transaction(NAMESPACE, (tx) => tx.set("sent", next.sent));
            })
            .catch((error: unknown) => context.log.error("nts push", error));
        };
        const stops = [needs.subscribe(check), issues.subscribe(check)];
        context.onStop(async () => {
          for (const stop of stops) stop();
          await chain;
        });
        check();
      },
    },
    exposeSource({
      ref: refs.pushKey,
      requires: [],
      output: z.object({ publicKey: z.string() }),
      resolve: () => publicKey,
    }),
    exposeOperation({
      ref: refs.pushSubscribe,
      requires: [pushService],
      input: pushDevice,
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(pushService).subscribe(input);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.pushUnsubscribe,
      requires: [pushService],
      input: z.object({ endpoint: z.string().url() }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(pushService).unsubscribe(input.endpoint);
        return { ok: true as const };
      },
    }),
    exposeOperation({
      ref: refs.pushTest,
      requires: [pushService],
      input: z.object({ endpoint: z.string().url() }),
      output: z.object({ ok: z.literal(true) }),
      run: async (input, context) => {
        await context.get(pushService).test(input.endpoint);
        return { ok: true as const };
      },
    }),
  ];
}

/** A device's push subscription, as the browser's PushSubscription.toJSON() gives it. */
const pushDevice = z.object({
  endpoint: z
    .string()
    .url()
    .refine((url) => url.startsWith("https://"), "A push endpoint is an https URL"),
  keys: z.object({ p256dh: z.string().min(1), auth: z.string().min(1) }),
});
