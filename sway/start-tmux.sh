#!/usr/bin/zsh

# Source .zshrc to load environment variables
source ~/dotfiles/zsh/exports.sh

# Automatically start or attach to a tmux session on login if not already in one
if command -v tmux &>/dev/null && [ -z "$TMUX" ]; then
  tmux new-session -s default &
fi
