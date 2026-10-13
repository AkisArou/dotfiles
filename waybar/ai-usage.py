#!/usr/bin/env python3
"""Read Claude, Codex and Muse subscription quotas using their existing logins."""

import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
import fcntl
import hashlib
import html
import json
import math
import os
from pathlib import Path
import re
import shutil
import tempfile
import time
import urllib.error
import urllib.request
from xml.etree import ElementTree

PROVIDERS = (("claude", "Claude"), ("openai", "Codex"), ("muse", "Muse"))
ROBOT = "󰚩"
BAR_WIDTH = 12
CACHE_TTL = 300
HTTP_TIMEOUT = 12


class UsageError(Exception):
    """A safe, user-facing error that contains no credentials or response body."""


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, message, headers, new_url):
        return None


HTTP = urllib.request.build_opener(NoRedirect())


def read_json(filename):
    try:
        value = json.loads(filename.read_text())
        return value if isinstance(value, dict) else {}
    except (OSError, ValueError):
        return {}


def credentials(provider):
    home = Path.home()
    headers = {"Accept": "application/json", "Content-Type": "application/json"}
    body = None
    plan = ""
    if provider == "claude":
        directory = Path(os.environ.get("CLAUDE_CONFIG_DIR") or home / ".claude")
        auth = read_json(directory / ".credentials.json").get("claudeAiOauth") or {}
        token = auth.get("accessToken")
        plan = str(auth.get("subscriptionType") or "").title()
        tier = str(auth.get("rateLimitTier") or "")
        if plan == "Max":
            plan += " 20x" if "20x" in tier else " 5x" if "5x" in tier else ""
        # Use the installed Claude release when it is a versioned native binary.
        version = Path(shutil.which("claude") or "claude").resolve().name
        agent = f"claude-cli/{version} (external, cli)" if re.fullmatch(r"\d+\.\d+\.\d+", version) else "dotfiles-ai-usage"
        headers.update({"anthropic-beta": "oauth-2025-04-20", "User-Agent": agent})
        url = "https://api.anthropic.com/api/oauth/usage"
    elif provider == "openai":
        directory = Path(os.environ.get("CODEX_HOME") or home / ".codex")
        auth = read_json(directory / "auth.json").get("tokens") or {}
        token = auth.get("access_token")
        if auth.get("account_id"):
            headers["ChatGPT-Account-Id"] = auth["account_id"]
        url = "https://chatgpt.com/backend-api/wham/usage"
    else:
        directory = Path(os.environ.get("XDG_CONFIG_HOME") or home / ".config")
        filename = Path(os.environ.get("MUSE_AUTH_PATH") or directory / "muse/auth.json")
        auth = (read_json(filename).get("providers") or {}).get("meta") or {}
        token = auth.get("access_token")
        headers["x-api-version"] = "1.0.0"
        url = "https://api.meta.ai/muse-code/key"
        # The CLI reads quota from this standing-key response without onboarding.
        body = b'{"onboard":false}'
    if not isinstance(token, str) or not token.strip():
        raise UsageError("Sign in with " + {"claude": "claude", "openai": "codex login", "muse": "muse login"}[provider])
    headers["Authorization"] = "Bearer " + token
    # Scope cached usage to the login without storing the token or account ID.
    identity = hashlib.sha256((token + headers.get("ChatGPT-Account-Id", "")).encode()).hexdigest()
    return url, headers, body, plan, identity


def fetch_json(url, headers, body):
    try:
        request = urllib.request.Request(url, headers=headers, data=body)
        with HTTP.open(request, timeout=HTTP_TIMEOUT) as response:
            payload = response.read(1024 * 1024 + 1)
        if len(payload) > 1024 * 1024:
            raise UsageError("Usage response is too large")
        result = json.loads(payload)
        if not isinstance(result, dict):
            raise UsageError("Unexpected usage response")
        return result
    except urllib.error.HTTPError as error:
        if error.code in (401, 403):
            raise UsageError("Login rejected; reopen the provider CLI to sign in") from None
        if error.code == 429:
            raise UsageError("Rate limited; retrying in a few minutes") from None
        raise UsageError(f"Usage request failed (HTTP {error.code})") from None
    except (OSError, ValueError):
        raise UsageError("Could not reach the usage service") from None


def timestamp(value):
    if value is None:
        return None
    try:
        if isinstance(value, str) and not value.replace(".", "", 1).isdigit():
            return datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
        result = float(value)
        return result if math.isfinite(result) and result > 0 else None
    except (ValueError, TypeError, OverflowError):
        return None


def window(label, value, percent_field, now):
    if not isinstance(value, dict) or value.get(percent_field) is None:
        return None
    used = value[percent_field]
    if isinstance(used, bool):
        raise UsageError("Unexpected usage percentage")
    used = float(used)
    if not math.isfinite(used) or used < 0:
        raise UsageError("Unexpected usage percentage")
    reset = timestamp(value.get("resets_at") or value.get("reset_at"))
    if reset is None and value.get("reset_after_seconds") is not None:
        remaining = float(value["reset_after_seconds"])
        if not math.isfinite(remaining) or remaining < 0:
            raise UsageError("Unexpected reset time")
        reset = now + remaining
    return {"label": label, "percent": used, "reset_at": reset}


def reset_credits(value):
    if not isinstance(value, dict):
        return None
    count = value.get("available_count")
    if isinstance(count, bool) or not isinstance(count, int) or count < 0:
        return None
    credits = []
    reported = value.get("credits")
    for credit in reported if isinstance(reported, list) else []:
        if not isinstance(credit, dict) or credit.get("status") != "available":
            continue
        title = credit.get("title")
        credits.append({
            "title": title if isinstance(title, str) and title else "Reset credit",
            "expires_at": timestamp(credit.get("expires_at")),
        })
    # Redemption IDs never leave the response; only descriptive fields are cached.
    return {"available": count, "credits": credits}


def parse_usage(provider, payload, plan, now):
    metrics = []
    note = ""
    if provider == "claude":
        if not any(key in payload for key in ("five_hour", "seven_day")):
            raise UsageError("Unexpected Claude usage response")
        for key, label in (("five_hour", "5-hour"), ("seven_day", "Weekly"), ("seven_day_sonnet", "Sonnet")):
            metric = window(label, payload.get(key), "utilization", now)
            if metric:
                metrics.append(metric)
        # Model-scoped limits (including Fable) are intentionally excluded.
    elif provider == "openai":
        if "rate_limit" not in payload:
            raise UsageError("Unexpected OpenAI usage response")
        plan = str(payload.get("plan_type") or "").replace("_", " ").title()
        limits = payload.get("rate_limit") or {}
        for key, fallback in (("primary_window", 18000), ("secondary_window", 604800)):
            value = limits.get(key)
            if not isinstance(value, dict):
                continue
            seconds = float(value.get("limit_window_seconds") or fallback)
            if not math.isfinite(seconds) or seconds <= 0:
                raise UsageError("Unexpected usage window")
            label = "Weekly" if seconds == 604800 else f"{seconds / 3600:g}-hour"
            metric = window(label, value, "used_percent", now)
            if metric:
                metrics.append(metric)
        metrics.sort(key=lambda metric: metric["label"] == "Weekly")
    else:
        plan = str(payload.get("subs_tier_name") or "").removeprefix("Muse Code ")
        if payload.get("is_subs_active") is not True:
            note = "No active Muse subscription"
        else:
            usage = payload.get("subs_usage") or {}
            for key, label in (("window", "5-hour"), ("weekly", "Weekly")):
                metric = window(label, usage.get(key), "used_percent", now)
                if metric:
                    metrics.append(metric)
            if not metrics:
                note = "Muse did not report usage; the window may be idle"
    entry = {"id": provider, "plan": plan, "metrics": metrics, "note": note}
    if provider == "openai":
        entry["reset_credits"] = reset_credits(payload.get("rate_limit_reset_credits"))
    return entry


def write_cache(filename, value):
    descriptor, temporary = tempfile.mkstemp(prefix=".usage-", dir=filename.parent)
    try:
        with os.fdopen(descriptor, "w") as output:
            json.dump(value, output, allow_nan=False)
        os.replace(temporary, filename)
    finally:
        Path(temporary).unlink(missing_ok=True)


def load_provider(provider):
    empty = {"id": provider, "metrics": []}
    try:
        url, headers, body, plan, identity = credentials(provider)
        directory = Path(os.environ.get("XDG_CACHE_HOME") or Path.home() / ".cache") / "waybar-ai-usage"
        directory.mkdir(mode=0o700, parents=True, exist_ok=True)
        filename = directory / f"{provider}.json"
        with (directory / f"{provider}.lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            cached = read_json(filename)
            if cached.get("identity") != identity:
                cached = {}
            now = time.time()
            entry = cached.get("entry")
            has_reset_data = provider != "openai" or isinstance(entry, dict) and "reset_credits" in entry
            if 0 <= now - cached.get("checked_at", 0) < CACHE_TTL and isinstance(entry, dict) and has_reset_data:
                return cached["entry"]
            try:
                payload = fetch_json(url, headers, body)
                entry = parse_usage(provider, payload, plan, now)
                resets = entry.get("reset_credits")
                if resets and resets["available"] > 0:
                    try:
                        details = fetch_json(url.removesuffix("/usage") + "/rate-limit-reset-credits", headers, None)
                        details = reset_credits(details)
                        if details:
                            # The count stays consistent with the main usage response.
                            resets["credits"] = details["credits"]
                    except (UsageError, ValueError, TypeError, AttributeError):
                        pass  # Expiry is optional; keep quota and the available count.
                fetched_at = now
            except (UsageError, ValueError, TypeError, AttributeError) as exception:
                # Provider errors contain only our own messages, never raw bodies.
                error = str(exception) if isinstance(exception, UsageError) else "Unexpected usage response"
                entry = dict(cached.get("entry") or empty)
                entry.update({"error": error, "stale": bool(entry.get("metrics"))})
                entry.setdefault("plan", plan)
                if provider == "openai":
                    entry.setdefault("reset_credits", None)
                fetched_at = cached.get("fetched_at", 0)
            try:
                write_cache(filename, {"identity": identity, "checked_at": now, "fetched_at": fetched_at, "entry": entry})
            except OSError:
                pass  # Still show the live result if caching is unavailable.
            return entry
    except (UsageError, OSError, TypeError, AttributeError) as error:
        return {**empty, "error": str(error) if isinstance(error, UsageError) else "Cannot read login or usage cache"}


def countdown(reset_at, now):
    if reset_at is None:
        return "—"
    seconds = max(0, int(reset_at - now))
    if seconds == 0:
        return "now"
    minutes = (seconds + 59) // 60
    days, minutes = divmod(minutes, 1440)
    hours, minutes = divmod(minutes, 60)
    if days:
        return f"{days}d {hours}h"
    return f"{hours}h {minutes:02d}m" if hours else f"{minutes}m"


def local_datetime(value):
    if value is None:
        return "—"
    try:
        return datetime.fromtimestamp(value).astimezone().strftime("%a %d %b, %H:%M %Z")
    except (ValueError, OSError, OverflowError):
        return "—"


def provider_tooltip(entry, label):
    heading = f"<span foreground='#569CD6'><b>{html.escape(label)}</b></span>"
    plan = entry.get("plan") or ""
    for prefix in ("Claude ", "ChatGPT ", "Muse Code "):
        plan = plan.removeprefix(prefix)
    if plan:
        heading += f"  <span foreground='#858585'>{html.escape(plan)}</span>"
    lines = [heading]

    if entry.get("stale"):
        lines.append("Showing cached usage")
    if entry.get("error"):
        lines.append(html.escape(str(entry["error"])))
    if entry.get("note"):
        lines.append(html.escape(entry["note"]))

    rows = []
    for metric in entry.get("metrics", []):
        percent = metric.get("percent")
        name = metric["label"]
        if percent is None or name == "5-hour" or "fable" in name.casefold():
            continue
        rows.append((name, percent, metric.get("reset_at")))

    if rows:
        label_width = max(8, *(len(name) for name, _, _ in rows))
        header = f"{'Window':<{label_width}}  {'Usage':<{BAR_WIDTH}}  {'Used':>4}  Resets in"
        table = [f"<span foreground='#858585'>{html.escape(header)}</span>"]
        for name, percent, reset_at in rows:
            filled = round(max(0, min(100, percent)) * BAR_WIDTH / 100)
            bar = (
                f"<span foreground='#569CD6'>{'█' * filled}</span>"
                f"<span foreground='#444444'>{'░' * (BAR_WIDTH - filled)}</span>"
            )
            table.append(
                f"{html.escape(name.ljust(label_width))}  {bar}  "
                f"<b>{percent:>3g}%</b>  {html.escape(countdown(reset_at, time.time()))}"
            )
            if reset_at is not None:
                reset_date = f"{'Reset':<{label_width}}  {local_datetime(reset_at)}"
                table.append(f"<span foreground='#858585'>{html.escape(reset_date)}</span>")
        lines.append("<tt>" + "\n".join(table) + "</tt>")

    resets = entry.get("reset_credits")
    if resets is not None:
        count = resets["available"]
        lines.append(f"<span foreground='#6a9955'><b>{count}</b> reset{'s' if count != 1 else ''} available</span>")
        if count:
            for credit in resets["credits"]:
                detail = credit["title"]
                if credit["expires_at"] is not None:
                    detail += f" · expires {local_datetime(credit['expires_at'])}"
                lines.append(f"<span foreground='#858585'>{html.escape(detail)}</span>")
            if not resets["credits"]:
                lines.append("<span foreground='#858585'>Expiry unavailable</span>")

    if not rows and not entry.get("error") and not entry.get("note"):
        lines.append("Usage unavailable")
    return "\n".join(lines)


def tooltip(report):
    entries = {entry["id"]: entry for entry in report.get("entries", [])}
    return "\n\n".join(
        provider_tooltip(
            entries.get(vendor, {"error": "Usage unavailable"}), label
        )
        for vendor, label in PROVIDERS
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pretty", action="store_true", help="Print a terminal usage summary")
    options = parser.parse_args()
    with ThreadPoolExecutor(max_workers=len(PROVIDERS)) as pool:
        entries = list(pool.map(load_provider, (provider for provider, _ in PROVIDERS)))
    details = tooltip({"entries": entries})
    if options.pretty:
        print("".join(ElementTree.fromstring("<tooltip>" + details + "</tooltip>").itertext()))
        if os.isatty(0):
            try:
                input("\nPress Enter to close…")
            except (EOFError, KeyboardInterrupt):
                pass
    else:
        print(json.dumps({"text": ROBOT, "tooltip": details}, ensure_ascii=False))


if __name__ == "__main__":
    main()
