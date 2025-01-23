if [ -f ~/.env ]; then
  export $(cat ~/.env | xargs)
fi

. ~/dotfiles/zsh/fzf.sh

export THEME="${THEME:-vscode}"

export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/usr/bin/go/bin:$PATH"
export PATH="/opt/asdf-vm/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/neovim/bin:$PATH"

export ANDROID_HOME="$HOME/Android/Sdk"
# export ANDROID_AVD_HOME="$HOME/.config/.android/avd"

export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools
# export PATH=$ANDROID_HOME/cmdline-tools/latest/bin/:$PATH

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_RUNTIME_DIR=/run/user/$(id -u)

export XKB_DEFAULT_LAYOUT="us,gr"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle"

export EDITOR="vim"
export VISUAL="vim"
export BROWSER="brave"
export MANPAGER="vim +Man!"
export MANWIDTH=999

export QT_SCALE_FACTOR=2
export QT_FONT_DPI=243

export NODE_OPTIONS="--max-old-space-size=8192 --disable-warning=ExperimentalWarning --experimental-transform-types"
export NODE_COMPILE_CACHE="$HOME/.cache/node-compile-cache"

export DESKTOP="192.168.1.2"
export LAPTOP="192.168.1.3"
