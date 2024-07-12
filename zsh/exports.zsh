export BUN_INSTALL="$HOME/.bun"

export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/usr/bin/go/bin:$PATH"
export PATH="$HOME/.local/share/neovim/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

export ANDROID_HOME="$HOME/Android/Sdk"
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DATA_DIRS="/usr/local/share:/usr/share:/var/lib/snapd/desktop"
export XDG_CONFIG_DIRS="/etc/xdg"

export XKB_DEFAULT_LAYOUT="us,gr"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle"

export TERMINAL="alacritty"
export TERM="xterm-256color"
export EDITOR="nvim"
export VISUAL="nvim"
export LAUNCH_EDITOR="/home/$(echo $USER)/dotfiles/scripts/launch-editor.sh"
export BROWSER="brave"
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export GTK_THEME="Dracula"
export QT_SCALE_FACTOR=2
export QT_FONT_DPI=243
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

export NODE_OPTIONS="--max-old-space-size=8192"

export DESKTOP="192.168.1.2"
export LAPTOP="192.168.1.3"

export MOZ_USE_XINPUT2=1

if [[ "$HOST" == "archlinux-xps" ]]; then
  export VDPAU_DRIVER=va_gl
fi

if [ -f ~/.env ]; then
    export $(cat ~/.env | xargs)
fi
