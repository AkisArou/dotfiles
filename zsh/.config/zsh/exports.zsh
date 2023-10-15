#!/bin/sh
# HISTFILE="$XDG_DATA_HOME"/zsh/history
HISTSIZE=1000000
SAVEHIST=1000000
export EDITOR="nvim"
export TERMINAL="kitty"
export BROWSER="brave"
export PATH="$HOME/.local/bin":$PATH
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export PATH="$HOME/.local/share/neovim/bin":$PATH
export XDG_CURRENT_DESKTOP="Wayland"
export PATH="/usr/lib/ccache/bin/:$PATH"
