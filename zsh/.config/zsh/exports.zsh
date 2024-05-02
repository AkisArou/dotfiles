# HISTFILE="$XDG_DATA_HOME"/zsh/history
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin":$PATH
export PATH="/usr/bin":$PATH
export PATH="/usr/lib/ccache/bin/:$PATH"
export PATH="/usr/bin/go/bin":$PATH
export PATH="$HOME/.local/share/neovim/bin":$PATH
export PATH="$HOME/.config/emacs/bin":$PATH

export XDG_CONFIG_HOME="$HOME/.config"
export XKB_DEFAULT_LAYOUT="us,gr"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle"
export TERMINAL="alacritty"
export EDITOR="nvim"
export LAUNCH_EDITOR="/home/$(echo $USER)/dotfiles/scripts/launch-editor.sh"
export BROWSER="brave"
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export ANDROID_HOME="$HOME/Android/Sdk"
export GTK_THEME="Colorful-Dark-GTK"
export GTK_PATH="$HOME/dotfiles/gtk"
export QT_SCALE_FACTOR=2
export QT_FONT_DPI=243
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
#export XDG_CURRENT_DESKTOP="Wayland"
#export XDG_SESSION_DESKTOP="hyprland"
#export XDG_SESSION_TYPE="wayland"


export GH_TOKEN=$(gh auth token)

# Mozilla specific for wayland
# export MOZ_ENABLE_WAYLAND=1
# export MOZ_WAYLAND_USE_VAAPI=1
# export MOZ_DBUS_REMOTE=1
