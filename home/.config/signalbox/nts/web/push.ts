// Notifications on this device (host/push.ts sends them; sw.js shows them). An
// iPhone receives them only in the app added to its home screen, and asks for
// permission only after a tap.
import { createContext, useCallback, useContext, useEffect, useState } from "react";
import { useCommand, useSource } from "signalbox/client/react";
import * as refs from "../shared/model.ts";
import type { PushDevice } from "../shared/model.ts";

/** Where this device stands: why it cannot, or whether it is on. */
export type PushState =
  | "unsupported" // no push in this browser
  | "install" // an iPhone or iPad in a browser tab: push needs the home-screen app
  | "blocked" // permission was refused; only the system settings can change it
  | "off"
  | "on";

const ios = () => /iPhone|iPad|iPod/.test(navigator.userAgent);
const standalone = () =>
  window.matchMedia("(display-mode: standalone)").matches ||
  (navigator as { standalone?: boolean }).standalone === true;

function supported(): boolean {
  return "serviceWorker" in navigator && "PushManager" in window && "Notification" in window;
}

/** A base64url key as the bytes PushManager.subscribe takes. */
function keyBytes(key: string): Uint8Array<ArrayBuffer> {
  const base64 = (key + "=".repeat((4 - (key.length % 4)) % 4))
    .replace(/-/g, "+")
    .replace(/_/g, "/");
  const raw = atob(base64);
  const bytes = new Uint8Array(new ArrayBuffer(raw.length));
  for (let i = 0; i < raw.length; i++) bytes[i] = raw.charCodeAt(i);
  return bytes;
}
const sameKey = (subscription: PushSubscription, key: string) => {
  const own = subscription.options.applicationServerKey;
  if (!own) return false;
  const expected = keyBytes(key);
  const actual = new Uint8Array(own);
  return actual.length === expected.length && actual.every((byte, i) => byte === expected[i]);
};
const device = (subscription: PushSubscription): PushDevice => {
  const json = subscription.toJSON();
  return { endpoint: json.endpoint!, keys: { p256dh: json.keys!.p256dh!, auth: json.keys!.auth! } };
};

export function usePush() {
  const key = useSource(refs.pushKey);
  const publicKey = key.status === "ready" ? key.value.publicKey : undefined;
  const subscribe = useCommand(refs.pushSubscribe);
  const unsubscribe = useCommand(refs.pushUnsubscribe);
  const sendTest = useCommand(refs.pushTest);
  const [state, setState] = useState<PushState>(() =>
    !supported() ? (ios() && !standalone() ? "install" : "unsupported") : "off",
  );
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string>();

  // On every start: a granted device stays known to the host, under the host's current key.
  useEffect(() => {
    if (!supported() || !publicKey) return;
    let cancelled = false;
    void (async () => {
      if (Notification.permission === "denied") return void (!cancelled && setState("blocked"));
      const registration = await navigator.serviceWorker.ready;
      let subscription = await registration.pushManager.getSubscription();
      if (subscription && !sameKey(subscription, publicKey)) {
        await subscription.unsubscribe();
        subscription =
          Notification.permission === "granted"
            ? await registration.pushManager.subscribe({
                userVisibleOnly: true,
                applicationServerKey: keyBytes(publicKey),
              })
            : null;
      }
      if (subscription) await subscribe.run(device(subscription));
      if (!cancelled) setState(subscription ? "on" : "off");
    })().catch((failure: unknown) => !cancelled && setError(String(failure)));
    return () => {
      cancelled = true;
    };
    // subscribe.run is stable; the key decides when to check again.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [publicKey]);

  /** Asks for permission (call it from a tap), subscribes, and sends a test notification. */
  const enable = useCallback(async () => {
    if (!publicKey) return;
    setBusy(true);
    setError(undefined);
    try {
      const permission = await Notification.requestPermission();
      if (permission !== "granted") return setState(permission === "denied" ? "blocked" : "off");
      const registration = await navigator.serviceWorker.ready;
      const subscription =
        (await registration.pushManager.getSubscription()) ??
        (await registration.pushManager.subscribe({
          userVisibleOnly: true,
          applicationServerKey: keyBytes(publicKey),
        }));
      await subscribe.run(device(subscription));
      setState("on");
      await sendTest.run({ endpoint: subscription.endpoint });
    } catch (failure) {
      setError(failure instanceof Error ? failure.message : String(failure));
    } finally {
      setBusy(false);
    }
  }, [publicKey, subscribe, sendTest]);

  const disable = useCallback(async () => {
    setBusy(true);
    setError(undefined);
    try {
      const registration = await navigator.serviceWorker.ready;
      const subscription = await registration.pushManager.getSubscription();
      if (subscription) {
        await unsubscribe.run({ endpoint: subscription.endpoint });
        await subscription.unsubscribe();
      }
      setState("off");
    } catch (failure) {
      setError(failure instanceof Error ? failure.message : String(failure));
    } finally {
      setBusy(false);
    }
  }, [unsubscribe]);

  const test = useCallback(async () => {
    const registration = await navigator.serviceWorker.ready;
    const subscription = await registration.pushManager.getSubscription();
    if (subscription) await sendTest.run({ endpoint: subscription.endpoint });
  }, [sendTest]);

  return { state, busy, error, ready: Boolean(publicKey), enable, disable, test };
}

/** The home-screen icon's badge: how many things wait on you, while the app is open. */
export function useAppBadge(count: number) {
  useEffect(() => {
    const badge = navigator as {
      setAppBadge?(n: number): Promise<void>;
      clearAppBadge?(): Promise<void>;
    };
    if (!badge.setAppBadge) return;
    void (count > 0 ? badge.setAppBadge(count) : badge.clearAppBadge?.())?.catch(() => {});
  }, [count]);
}

const DISMISSED = "nts-push-prompt-dismissed";
/** Whether to offer notifications unasked: once, until turned on or put off. */
export function usePushPrompt(state: PushState) {
  const [dismissed, setDismissed] = useState(() => {
    try {
      return localStorage.getItem(DISMISSED) === "1";
    } catch {
      return false;
    }
  });
  const dismiss = useCallback(() => {
    setDismissed(true);
    try {
      localStorage.setItem(DISMISSED, "1");
    } catch {
      // Private browsing: the prompt comes back next time, which is harmless.
    }
  }, []);
  const show =
    !dismissed &&
    state === "off" &&
    typeof Notification !== "undefined" &&
    Notification.permission === "default";
  return { show, dismiss };
}

export type Push = ReturnType<typeof usePush>;
/** One push state for the page (the shell makes it): its prompt, the More sheet and Settings. */
export const PushContext = createContext<Push | undefined>(undefined);
export function usePushContext(): Push {
  const push = useContext(PushContext);
  if (!push) throw new Error("usePushContext outside the shell");
  return push;
}
