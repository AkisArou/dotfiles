// Where the browser finds the host's API, from where it loaded the page: one
// build serves the host itself, an SSH tunnel and a proxy (tailscale serve).

/**
 * An explicit API URL wins. A page opened on the web port (the host's own address,
 * or an SSH tunnel to it) finds the API on the API port of the same host name. A page
 * opened through a proxy finds it on the same address, at /rpc (the proxy routes /rpc
 * and /terminals to the API port).
 */
export function apiAddress(
  page: {
    readonly protocol: string;
    readonly hostname: string;
    readonly host: string;
    readonly port: string;
  },
  ports: { readonly web: number; readonly api: number },
  explicit = "",
): string {
  if (explicit) return explicit;
  const scheme = page.protocol === "https:" ? "wss:" : "ws:";
  // A URL's hostname keeps an IPv6 address in its brackets.
  return page.port === String(ports.web)
    ? `${scheme}//${page.hostname}:${ports.api}/rpc`
    : `${scheme}//${page.host}/rpc`;
}
