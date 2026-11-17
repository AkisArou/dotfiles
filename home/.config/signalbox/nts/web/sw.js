// Cache only this app's shell. RPC, sockets and mutations are never cached or queued.
const CACHE = "signalbox-nts-shell-v2";
const SHELL = [
  "/",
  "/app.js",
  "/app.css",
  "/manifest.webmanifest",
  "/icons/icon-192.png",
  "/icons/icon-512.png",
];
self.addEventListener("install", (event) => {
  event.waitUntil(caches.open(CACHE).then((cache) => cache.addAll(SHELL)));
  self.skipWaiting();
});
self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter(
              (key) =>
                (key.startsWith("signalbox-lab-shell-") ||
                  key.startsWith("signalbox-nts-shell-")) &&
                key !== CACHE,
            )
            .map((key) => caches.delete(key)),
        ),
      )
      .then(() => self.clients.claim()),
  );
});
self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (
    event.request.method !== "GET" ||
    url.origin !== self.location.origin ||
    !SHELL.includes(url.pathname)
  )
    return;
  event.respondWith(
    fetch(event.request)
      .then(async (response) => {
        if (response.ok) {
          const cache = await caches.open(CACHE);
          await cache.put(event.request, response.clone());
        }
        return response;
      })
      .catch(async () => {
        const cached = await caches.match(event.request);
        return (
          cached ??
          new Response("The application shell is unavailable offline.", {
            status: 503,
            headers: { "Content-Type": "text/plain" },
          })
        );
      }),
  );
});

// A notification from the host (host/push.ts): what now waits on the owner. The
// home-screen icon's badge shows how many things wait.
self.addEventListener("push", (event) => {
  let notice;
  try {
    notice = event.data.json();
  } catch {
    notice = { title: "Needs you", body: event.data ? event.data.text() : "", url: "/needs-you" };
  }
  event.waitUntil(
    Promise.all([
      self.registration.showNotification(notice.title, {
        body: notice.body,
        tag: notice.id,
        icon: "/icons/icon-192.png",
        badge: "/icons/icon-192.png",
        data: { url: notice.url ?? "/needs-you" },
      }),
      typeof notice.count === "number" && self.navigator.setAppBadge
        ? notice.count > 0
          ? self.navigator.setAppBadge(notice.count)
          : self.navigator.clearAppBadge()
        : Promise.resolve(),
    ]),
  );
});
// Tapping one opens the app where it points, in the window already open if there is one.
self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const url = new URL(event.notification.data?.url ?? "/needs-you", self.location.origin).href;
  event.waitUntil(
    self.clients.matchAll({ type: "window", includeUncontrolled: true }).then((windows) => {
      const open = windows.find((each) => new URL(each.url).origin === self.location.origin);
      if (open) return open.navigate(url).then((client) => (client ?? open).focus());
      return self.clients.openWindow(url);
    }),
  );
});
