#!/usr/bin/env python3
"""Show a robot in the bar and Claude/Codex usage meters in one tooltip."""

import html
import json
import subprocess

PROVIDERS = (("anthropic", "Claude"), ("openai", "OpenAI / Codex"))
ROBOT = "󰚩"
BAR_WIDTH = 12


def provider_tooltip(entry, label):
    heading = f"<span foreground='#569CD6'><b>{html.escape(label)}</b></span>"
    plan = entry.get("plan") or ""
    for prefix in ("Claude ", "ChatGPT "):
        plan = plan.removeprefix(prefix)
    if plan:
        heading += f"  <span foreground='#858585'>{html.escape(plan)}</span>"
    lines = [heading]

    if entry.get("stale"):
        lines.append("Showing cached usage")
    if entry.get("error"):
        lines.append(html.escape(str(entry["error"])))

    rows = []
    for metric in entry.get("metrics", []):
        percent = metric.get("percent")
        name = metric["label"]
        if percent is None or "fable" in name.casefold():
            continue
        name = {
            "Session (5h)": "5-hour",
            "Codex (5h)": "5-hour",
            "Weekly (7d)": "Weekly",
            "Codex weekly": "Weekly",
        }.get(name, name)
        # The rest of the detail is pacing advice; keep just the reset timer.
        detail = (metric.get("detail") or "").split(" · ", 1)[0]
        reset = detail.removeprefix("Resets in ") if detail.startswith("Resets in ") else "—"
        rows.append((name, percent, reset))

    if rows:
        label_width = max(8, *(len(name) for name, _, _ in rows))
        header = f"{'Window':<{label_width}}  {'Usage':<{BAR_WIDTH}}  {'Used':>4}  Resets in"
        table = [f"<span foreground='#858585'>{html.escape(header)}</span>"]
        for name, percent, reset in rows:
            filled = round(max(0, min(100, percent)) * BAR_WIDTH / 100)
            bar = (
                f"<span foreground='#569CD6'>{'█' * filled}</span>"
                f"<span foreground='#444444'>{'░' * (BAR_WIDTH - filled)}</span>"
            )
            table.append(
                f"{html.escape(name.ljust(label_width))}  {bar}  "
                f"<b>{percent:>3g}%</b>  {html.escape(reset)}"
            )
        lines.append("<tt>" + "\n".join(table) + "</tt>")

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

    print(json.dumps({"text": ROBOT, "tooltip": details}, ensure_ascii=False))


if __name__ == "__main__":
    main()
