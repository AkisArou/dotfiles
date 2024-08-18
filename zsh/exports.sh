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
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

export XKB_DEFAULT_LAYOUT="us,gr"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle"

export TERMINAL="st"
export TERM="xterm-256color"
export EDITOR="nvim"
export VISUAL="nvim"
export LAUNCH_EDITOR="/home/$(echo "$USER")/dotfiles/scripts/launch-editor.sh"
export BROWSER="brave"
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export GTK_THEME="Dracula"
export QT_SCALE_FACTOR=2
export QT_FONT_DPI=243
export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS \
  --highlight-line \
  --info=inline-right \
  --ansi \
  --layout=reverse \
  --border=none
  --color=bg+:#2e3c64 \
  --color=bg:#1f2335 \
  --color=border:#29a4bd \
  --color=fg:#c0caf5 \
  --color=gutter:#1f2335 \
  --color=header:#ff9e64 \
  --color=hl+:#2ac3de \
  --color=hl:#2ac3de \
  --color=info:#545c7e \
  --color=marker:#ff007c \
  --color=pointer:#ff007c \
  --color=prompt:#2ac3de \
  --color=query:#c0caf5:regular \
  --color=scrollbar:#29a4bd \
  --color=separator:#ff9e64 \
  --color=spinner:#ff007c \
"

export NODE_OPTIONS="--max-old-space-size=8192"

export DESKTOP="192.168.1.2"
export LAPTOP="192.168.1.3"

export MOZ_USE_XINPUT2=1
export MOZ_WEBRENDER=1
export MOZ_X11_EGL=1

if [[ "$HOST" == "arch-xps" ]]; then
  export VDPAU_DRIVER=va_gl
fi

if [ -f ~/.env ]; then
  export $(cat ~/.env | xargs)
fi
