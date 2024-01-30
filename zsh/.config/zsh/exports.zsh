#!/bin/sh
# HISTFILE="$XDG_DATA_HOME"/zsh/history
HISTSIZE=1000000
SAVEHIST=1000000
export EDITOR="nvim"
export LAUNCH_EDITOR="/home/$(echo $USER)/dotfiles/scripts/launch-editor.sh"
export TERMINAL="alacritty"
export BROWSER="brave"
export PATH="$HOME/.local/bin":$PATH
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export PATH="$HOME/.local/share/neovim/bin":$PATH
export XDG_CURRENT_DESKTOP="Wayland"
export XDG_SESSION_DESKTOP="hyprland"
export XDG_SESSION_TYPE="wayland"
export PATH="/usr/lib/ccache/bin/:$PATH"
export ANDROID_HOME="$HOME/Android/Sdk"
export XKB_DEFAULT_LAYOUT="us,gr"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle"

# Mozilla specific for wayland
# export MOZ_ENABLE_WAYLAND=1
# export MOZ_WAYLAND_USE_VAAPI=1
# export MOZ_DBUS_REMOTE=1

