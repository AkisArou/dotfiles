"""Record Codex IDs and save exact Codex, Claude, and Muse conversations."""

import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import uuid

from agent_runtime import PaneOptions


def tmux(*args):
    socket = os.environ.get("TMUX", "").rsplit(",", 2)[0]
    command = ["tmux", "-S", socket] if socket else ["tmux"]
    return subprocess.run(
        [*command, *args], check=True, capture_output=True, text=True, timeout=1,
    ).stdout.rstrip("\n")


def session_id(value):
    try:
        return str(uuid.UUID(value)) if isinstance(value, str) else None
    except ValueError:
        return None


def codex_hook():
    if not os.environ.get("TMUX"):
        sys.stdin.buffer.read()
        return
    event = json.load(sys.stdin)
    if not isinstance(event, dict) or event.get("agent_id"):
        return
    conversation = session_id(event.get("session_id"))
    name = event.get("hook_event_name")
    if not conversation or name not in ("SessionStart", "SessionEnd", "UserPromptSubmit"):
        return

    with PaneOptions("codex") as pane:
        previous = pane.get("@codex_session") if name != "SessionStart" else None
        current = previous
        if name == "SessionEnd":
            if previous == conversation:
                current = None
        elif name == "SessionStart" or previous in (None, conversation):
            current = conversation
        pane.apply({"@codex_session": current})


def foreground_agent(pane_pid, processes):
    """Inspect the pane's foreground group, excluding stale IDs and background jobs."""
    try:
        fields = (Path("/proc") / pane_pid / "stat").read_text().rsplit(")", 1)[1].split()
        group = fields[5]  # tty foreground process group, after the comm field
    except (OSError, IndexError):
        return None
    candidates = {}
    for pid, process_group, comm in processes:
        if process_group != group:
            continue
        try:
            args = (Path("/proc") / pid / "cmdline").read_bytes().decode().split("\0")[:-1]
        except (OSError, UnicodeError):
            continue
        if not args:
            continue
        agent = comm if comm in ("codex", "claude") else None
        if re.fullmatch(r"muse(-bin(-[0-9].*)?)?", Path(args[0]).name):
            agent = "muse"
        native = agent is not None
        if not agent and Path(args[0]).name == "node" and len(args) > 1:
            script = Path(args[1]).name
            if script in ("codex", "codex.js"):
                agent = "codex"
            elif script == "claude" or "/@anthropic-ai/claude-code/" in args[1]:
                agent = "claude"
            args = args[1:]
        if agent and (native or agent not in candidates):
            candidates[agent] = args[1:]
    if len(candidates) == 1:
        return next(iter(candidates.items()))
    return None


def resume_options(agent, args):
    """Keep explicit CLI configuration, without resending startup prompts or forking."""
    if agent == "codex":
        values = {
            "-c", "--config", "-p", "--profile", "-m", "--model", "-s", "--sandbox",
            "-a", "--ask-for-approval", "--local-provider", "--enable", "--disable",
            "--add-dir", "--remote", "--remote-auth-token-env",
        }
        flags = {
            "--oss", "--search", "--no-alt-screen", "--no-daemon", "--approve-for-me",
            "--dangerously-bypass-approvals-and-sandbox",
        }
    elif agent == "muse":
        values = {
            "--provider", "--preset", "-c", "--config", "--model", "--reasoning-effort",
            "--base-url", "--workspace", "-w", "--worktree", "--worktree-base",
            "--worktree-existing", "--approval-mode", "--permission-profile",
            "--approval-judge", "--echo-delay-ms", "--sandbox-network",
            "--context-compaction-strategy", "--context-compaction-soft-threshold",
            "--context-compaction-hard-threshold",
        }
        flags = {
            "--parallel-tool-calls", "--no-parallel-tool-calls", "--subagent-worktree-isolation",
            "--yolo", "--trust-workspace", "--disable-approval", "--disable-sandbox",
            "--disable-write", "--disable-shell", "--enable-shell-tool",
        }
    else:
        values = {
            "--model", "--permission-mode", "--settings", "--settings-sources",
            "--mcp-config", "--agent", "--add-dir", "--betas", "--effort",
        }
        flags = {
            "--dangerously-skip-permissions", "--allow-dangerously-skip-permissions",
            "--strict-mcp-config", "--chrome", "--no-chrome", "--ide",
        }
    result = []
    index = 0
    while index < len(args):
        arg = args[index]
        if arg == "--":
            break
        if agent == "muse" and arg in ("-w", "--worktree"):
            # Muse accepts this with no value; do not swallow the following flag.
            explicit = index + 1 < len(args) and args[index + 1] in ("off", "create", "existing")
            result.extend([arg, args[index + 1] if explicit else "create"])
            index += 2 if explicit else 1
            continue
        if arg in values and index + 1 < len(args):
            result.extend(args[index:index + 2])
            index += 2
            continue
        if arg in flags or ("=" in arg and arg.split("=", 1)[0] in values):
            result.append(arg)
        index += 1
    return result


def snapshot(path):
    output = subprocess.run(
        ["ps", "-eo", "pid=,pgid=,comm="],
        check=True, capture_output=True, text=True, timeout=1,
    ).stdout
    processes = [line.split(None, 2) for line in output.splitlines() if line.strip()]
    format = "\t".join((
        "#{session_name}", "#{window_index}", "#{pane_index}", "#{pane_id}",
        "#{pane_pid}", "#{@codex_session}", "#{@claude_state}", "#{@muse_state}",
    ))
    panes = {}
    for line in tmux("list-panes", "-a", "-F", format).splitlines():
        fields = line.split("\t", 7)
        if len(fields) == 8:
            panes[tuple(fields[:3])] = fields[3:]

    lines = path.read_text().splitlines()
    for index, line in enumerate(lines):
        fields = line.split("\t")
        if len(fields) != 11 or fields[0] != "pane":
            continue
        pane = panes.get((fields[1], fields[2], fields[5]))
        active = foreground_agent(pane[1], processes) if pane else None
        # Never launch a fresh conversation if its exact ID is unavailable.
        if fields[9] in ("codex", "claude", "muse"):
            fields[10] = ":"
        if active:
            agent, args = active
            conversation = pane[2]
            if agent in ("claude", "muse"):
                try:
                    conversation = json.loads(pane[3 if agent == "claude" else 4]).get("session")
                except (ValueError, AttributeError):
                    conversation = None
            conversation = session_id(conversation)
            fields[9], fields[10] = agent, ":"
            if conversation:
                resume = "--resume" if agent == "claude" else "resume"
                command = [agent, resume, conversation, *resume_options(agent, args)]
                if any("\n" in arg or "\t" in arg for arg in command):
                    print(f"Unsupported multiline {agent} option in pane {pane[0]}; saving its layout only.", file=sys.stderr)
                else:
                    fields[10] = ":" + shlex.join(command)
            else:
                print(f"No {agent} conversation ID recorded for pane {pane[0]}; saving its layout only.", file=sys.stderr)
        lines[index] = "\t".join(fields)
    # This is the new snapshot, before Resurrect compares it and updates `last`.
    path.write_text("\n".join(lines) + "\n")


def main():
    if sys.argv[1:] == ["codex-hook"]:
        try:
            codex_hook()
        except (OSError, ValueError, TypeError, subprocess.SubprocessError):
            pass  # Hook bookkeeping must not interfere with the agent.
    elif len(sys.argv) == 3 and sys.argv[1] == "snapshot":
        snapshot(Path(sys.argv[2]))
    else:
        raise SystemExit("Usage: agent-sessions.py codex-hook | snapshot <resurrect-file>")


if __name__ == "__main__":
    main()
