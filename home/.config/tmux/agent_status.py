"""Claude and Muse hooks: per-pane activity state alongside their native titles.

tmux renders working, attention, or error as one colored dot.
Ready, compaction, and background subagent counts have no extra marker.
No output or permission decisions: a missing tmux server must never block an agent.
"""

import json
import os
import subprocess
import sys

from agent_runtime import PaneOptions


def update(state, event):
    """Update only the requesting tool's wait, even when tools run in parallel."""
    name = event.get("hook_event_name")
    session = event.get("session_id")
    agent = event.get("agent_id")
    if not session:
        return state
    if name == "SessionStart" and not agent:
        if event.get("source") == "compact" and state.get("session") == session:
            return state
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
        if name in ("PreToolUse", "PreLLMCall"):
            if not agent:
                state["status"] = "working"
            if tool == "AskUserQuestion":
                waits[tool_key] = "?"
            elif tool == "ExitPlanMode":
                waits[tool_key] = "!"
        elif name == "PermissionRequest":
            if not event.get("tool_use_id"):
                # Muse omits the ID here. Count waits per tool for parallel calls.
                prefix = f'{agent or "main"}:permission:{tool}:'
                index = 1
                while f"{prefix}{index}" in waits:
                    index += 1
                tool_key = f"{prefix}{index}"
            waits[tool_key] = "?" if tool == "AskUserQuestion" else "!"
        elif name in ("PostToolUse", "PostToolUseFailure", "PermissionDenied"):
            waits.pop(tool_key, None)
            prefix = f'{agent or "main"}:permission:{tool}:'
            unresolved = next((key for key in waits if key.startswith(prefix)), None)
            waits.pop(unresolved, None)
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
    if not agent and name in ("PreToolUse", "PreLLMCall", "PostToolUse", "PostToolUseFailure", "PermissionDenied", "ElicitationResult"):
        waits.pop("main:notification", None)
    state["waits"] = waits
    state["agents"] = sorted(agents)
    return state


def badge(state):
    waits = set(state.get("waits", {}).values())
    if waits & {"?", "!"}:
        return "attention"
    if state.get("status") == "error":
        return "error"
    if state.get("status") == "working" and not state.get("compacting"):
        return "working"
    return ""


def muse_event(event):
    """Use Muse's top-level session identity and distinguish child thread hooks."""
    event = dict(event)
    session = os.environ.get("MUSE_SESSION_ID") or event.get("session_id")
    thread = os.environ.get("MUSE_THREAD_ID") or event.get("session_id")
    event["session_id"] = session
    event["prompt_id"] = event.get("turn_id")
    tools = {"request_user_input": "AskUserQuestion", "exit_plan_mode": "ExitPlanMode"}
    event["tool_name"] = tools.get(event.get("tool_name"), event.get("tool_name"))
    if thread and thread != session:
        event["agent_id"] = thread
    if event.get("hook_event_name") in ("SubagentStart", "SubagentStop"):
        event["agent_id"] = event.get("child_session_id") or event.get("subagent_id")
    return event


def main(agent):
    if not os.environ.get("TMUX"):
        sys.stdin.buffer.read()
        return
    event = json.load(sys.stdin)
    if not isinstance(event, dict):
        return
    if agent == "muse":
        event = muse_event(event)
    with PaneOptions(agent) as pane:
        fresh = (
            event.get("hook_event_name") == "SessionStart"
            and not event.get("agent_id") and event.get("source") != "compact"
        )
        raw = None if fresh else pane.get(f"@{agent}_state")
        previous = json.loads(raw) if raw else {}
        state = update(previous, event)
        pane.apply({
            f"@{agent}_state": json.dumps(state, separators=(",", ":")) if state else None,
            f"@{agent}_badge": badge(state) if state else None,
        })


def run(agent):
    try:
        main(agent)
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError):
        # Cosmetic integration only: stay silent and never affect the agent's work.
        pass
