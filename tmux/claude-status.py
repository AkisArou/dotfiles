"""Claude's tmux hook entry point; shared state handling lives in agent_status."""

from agent_status import run

if __name__ == "__main__":
    run("claude")
