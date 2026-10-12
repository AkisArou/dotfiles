"""Claude command hooks: per-pane attention badges, alongside its native title.

? question, ! approval, ✓ finished turn, × API error, ≋ compaction, +N subagents.
No output or permission decisions: a missing tmux server must never block Claude.
"""

import fcntl
import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path


def tmux(socket, *args):
    return subprocess.run(
        ["tmux", "-S", socket, *args],
        check=True, capture_output=True, text=True, timeout=0.5,
    ).stdout.strip()


def update(state, event):
    """Update only the requesting tool's wait, even when tools run in parallel."""
    name = event.get("hook_event_name")
    session = event.get("session_id")
    agent = event.get("agent_id")
    if not session:
        return state
    if name == "SessionStart" and not agent:
        return {"session": session, "status": "idle", "waits": {}, "agents": []}
    if not state:
        if agent or name == "SessionEnd":
            return state
        state = {"session": session, "status": "idle", "waits": {}, "agents": []}
    if state["session"] != session:
        return state  # A late event from a session replaced by /resume or /clear.
    if name == "SessionEnd" and not agent:
        return {}

    waits = state["waits"]
    agents = set(state["agents"])
    tool = event.get("tool_name")
    tool_key = f'{agent or "main"}:tool:{event.get("tool_use_id") or tool}'
    elicitation_key = f'{agent or "main"}:mcp:{event.get("mcp_server_name")}:{event.get("elicitation_id", "")}'

    if name == "SubagentStart":
        if agent:
            agents.add(agent)
    elif name == "SubagentStop":
        agents.discard(agent)
        waits = {key: value for key, value in waits.items() if not key.startswith(f"{agent}:")}
    elif name == "UserPromptSubmit" and not agent:
        state["status"] = "working"
        state["prompt"] = event.get("prompt_id")
        state.pop("compacting", None)
        waits = {key: value for key, value in waits.items() if not key.startswith("main:")}
    else:
        # Background subagents may still belong to an earlier prompt.
        if not agent and event.get("prompt_id") and state.get("prompt") not in (None, event["prompt_id"]):
            return state
        if name == "PreToolUse":
            if not agent:
                state["status"] = "working"
            if tool == "AskUserQuestion":
                waits[tool_key] = "?"
            elif tool == "ExitPlanMode":
                waits[tool_key] = "!"
        elif name == "PermissionRequest":
            waits[tool_key] = "?" if tool == "AskUserQuestion" else "!"
        elif name in ("PostToolUse", "PostToolUseFailure", "PermissionDenied"):
            waits.pop(tool_key, None)
            if not agent:
                state["status"] = "idle" if event.get("is_interrupt") else "working"
        elif name == "Elicitation":
            waits[elicitation_key] = "?"
        elif name == "ElicitationResult":
            waits.pop(elicitation_key, None)
        elif name == "PreCompact" and not agent:
            state["compacting"] = True
        elif name == "PostCompact" and not agent:
            state.pop("compacting", None)
        elif name in ("Stop", "StopFailure") and not agent:
            state["status"] = "error" if name == "StopFailure" else "done"
            state.pop("compacting", None)
            waits = {key: value for key, value in waits.items() if not key.startswith("main:")}
        elif name == "Notification" and not agent:
            notification = event.get("notification_type")
            # Fallbacks for network approval dialogs and interruptions without Stop.
            if notification == "permission_prompt":
                waits["main:notification"] = "!"
            elif notification == "idle_prompt":
                state["status"] = "done"
                state.pop("compacting", None)
                waits = {key: value for key, value in waits.items() if not key.startswith("main:")}

    # A delayed permission notification has no tool ID; any resumed main tool clears it.
    if not agent and name in ("PreToolUse", "PostToolUse", "PostToolUseFailure", "PermissionDenied", "ElicitationResult"):
        waits.pop("main:notification", None)
    state["waits"] = waits
    state["agents"] = sorted(agents)
    return state


def badge(state):
    waits = set(state.get("waits", {}).values())
    if "?" in waits:
        marker = "?"
    elif "!" in waits:
        marker = "!"
    elif state.get("compacting"):
        marker = "≋"
    else:
        marker = {"done": "✓", "error": "×"}.get(state.get("status"), "")
    parts = [f"[{marker}]"] if marker else []
    count = len(state.get("agents", []))
    if count:
        parts.append(f"[+{count}]")
    return " ".join(parts)


def main():
    socket = os.environ.get("TMUX", "").rsplit(",", 2)[0]
    pane = os.environ.get("TMUX_PANE", "")
    if not socket or not re.fullmatch(r"%\d+", pane):
        return
    event = json.load(sys.stdin)
    if not isinstance(event, dict):
        return
    runtime = Path(os.environ.get("XDG_RUNTIME_DIR") or Path.home() / ".cache")
    lock_dir = runtime / "dotfiles-tmux"
    lock_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
    server = hashlib.sha256(socket.encode()).hexdigest()[:16]
    # Hooks for parallel tools share one pane; serialize their read/modify/write.
    with (lock_dir / f"{server}-{pane[1:]}.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        raw = tmux(socket, "show-option", "-pqv", "-t", pane, "@claude_state")
        previous = json.loads(raw) if raw else {}
        state = update(previous, event)
        if state:
            tmux(socket, "set-option", "-p", "-t", pane, "@claude_state", json.dumps(state))
            tmux(socket, "set-option", "-p", "-t", pane, "@claude_badge", badge(state))
        else:
            tmux(socket, "set-option", "-pu", "-t", pane, "@claude_state")
            tmux(socket, "set-option", "-pu", "-t", pane, "@claude_badge")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError):
        # Cosmetic integration only: stay silent and never affect the agent's work.
        pass
