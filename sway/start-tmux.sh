#!/usr/bin/zsh

source ~/dotfiles/zsh/exports.sh

tmux new-session -s default -d &
tmux new-session -s work -d -c ~/nable-solutions &
