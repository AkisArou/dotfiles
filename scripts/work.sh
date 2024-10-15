#!/usr/bin/zsh

# Set the desired session name and working directory
SESSION_NAME="work"
WORK_DIR="$HOME/nable-solutions"

# Check if we're already in a tmux session
if [ -n "$TMUX" ]; then
  # If we're in tmux, check if the session exists
  if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    # Switch to the existing session
    echo "Switching to the existing tmux session '$SESSION_NAME'..."
    tmux switch-client -t "$SESSION_NAME"
  else
    # Create a new session if it doesn't exist
    echo "Creating a new tmux session named '$SESSION_NAME'..."
    tmux new-session -s "$SESSION_NAME" -c "$WORK_DIR"
  fi
else
  # If not in tmux, check if the session exists
  if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    # Attach to the existing session
    echo "Session '$SESSION_NAME' already exists. Attaching to it..."
    tmux attach-session -t "$SESSION_NAME"
  else
    # Create a new session if it doesn't exist
    echo "Creating a new tmux session named '$SESSION_NAME'..."
    tmux new-session -s "$SESSION_NAME" -c "$WORK_DIR"
  fi
fi
