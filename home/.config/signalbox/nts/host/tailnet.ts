// This machine's address on its tailnet, when Tailscale runs here: a page that
// tailscale serve proxies to the host comes from it, so the host accepts it.
// Read from Tailscale at start, so no machine's or tailnet's name lives in config.
import { execFile } from "node:child_process";
import { promisify } from "node:util";

/** The HTTPS origin `tailscale serve` gives this machine, from `tailscale status --json`. */
export function tailnetOrigin(status: unknown): string | undefined {
  const name = (status as { Self?: { DNSName?: unknown } } | null)?.Self?.DNSName;
  if (typeof name !== "string" || !name) return undefined;
  return `https://${name.replace(/\.$/, "")}`;
}

/** This machine's tailnet origin, or none when Tailscale is absent or not running. */
export async function readTailnetOrigins(): Promise<string[]> {
  try {
    const { stdout } = await promisify(execFile)("tailscale", ["status", "--json"], {
      timeout: 3000,
      maxBuffer: 4 * 1024 * 1024,
    });
    const origin = tailnetOrigin(JSON.parse(stdout));
    return origin ? [origin] : [];
  } catch {
    return [];
  }
}
