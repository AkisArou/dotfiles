#!/usr/bin/env python3
"""Keep the bar label quiet and put Claude/Codex quotas in one tooltip."""

import html
import json
import subprocess

PROVIDERS = (("anthropic", "Claude"), ("openai", "OpenAI / Codex"))


def provider_tooltip(entry, label):
    heading = label
    if entry.get("plan"):
        heading += f" · {entry['plan']}"
    lines = [f"<b>{html.escape(heading)}</b>"]

    if entry.get("stale"):
        lines.append("Showing cached usage")
    if entry.get("error"):
        lines.append(html.escape(str(entry["error"])))

    for metric in entry.get("metrics", []):
        percent = metric.get("percent")
        if percent is None:
            continue
        row = f"{metric['label']}: {percent:g}% used"
        # The rest of the detail is pacing advice; keep just the reset timer.
        reset = (metric.get("detail") or "").split(" · ", 1)[0]
        if reset.startswith("Resets"):
            row += f" · {reset.lower()}"
        lines.append(html.escape(row))

    if len(lines) == 1:
        lines.append("Usage unavailable")
    return "\n".join(lines)


def tooltip(report):
    entries = {entry["id"]: entry for entry in report.get("entries", [])}
    return "\n\n".join(
        provider_tooltip(
            entries.get(vendor, {"error": "Provider is not enabled"}), label
        )
        for vendor, label in PROVIDERS
    )


def main():
    try:
        result = subprocess.run(
            ["ai-usagebar", "usage", "--json"],
            capture_output=True,
            text=True,
            check=True,
            timeout=45,
        )
        details = tooltip(json.loads(result.stdout))
    except FileNotFoundError:
        details = "ai-usagebar is not installed"
    except subprocess.TimeoutExpired:
        details = "Usage refresh timed out"
    except (subprocess.CalledProcessError, ValueError, KeyError, TypeError, AttributeError):
        details = "Usage unavailable; run ai-usagebar usage for details"

    print(json.dumps({"text": "AI", "tooltip": details}, ensure_ascii=False))


if __name__ == "__main__":
    main()
