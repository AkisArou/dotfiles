import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { createClient, webSocketConnection } from "signalbox/client";
import { SignalboxProvider } from "signalbox/client/react";
import { RouterProvider } from "@tanstack/react-router";
import { router } from "./router.tsx";
import "@astryxdesign/core/reset.css";
import "@astryxdesign/core/astryx.css";
import "@astryxdesign/theme-neutral/theme.css";
import "./theme.css";
import { TerminalProvider } from "../shared/terminal.ts";
import { terminalUrl } from "signalbox/client/terminal";
import { apiAddress } from "../shared/address.ts";
import { followAppHeight } from "./viewport.ts";

declare const SIGNALBOX_API_URL: string;
declare const SIGNALBOX_WEB_PORT: number;
declare const SIGNALBOX_API_PORT: number;
// The API's address from where this page loaded (shared/address.ts), unless one is configured.
const api = apiAddress(
  window.location,
  { web: SIGNALBOX_WEB_PORT, api: SIGNALBOX_API_PORT },
  SIGNALBOX_API_URL,
);
followAppHeight();
const client = createClient({ connect: webSocketConnection(api) });
client.start();
const element = document.getElementById("root");
if (!element) throw new Error("Missing application root");
createRoot(element).render(
  <StrictMode>
    <SignalboxProvider client={client}>
      <TerminalProvider url={terminalUrl(api)}>
        <RouterProvider router={router} />
      </TerminalProvider>
    </SignalboxProvider>
  </StrictMode>,
);
window.addEventListener("pagehide", () => client.close());
window.addEventListener("pageshow", () => client.start());
if ("serviceWorker" in navigator)
  void navigator.serviceWorker
    .register("/sw.js")
    .catch((error) => console.error("App shell registration failed", error));
