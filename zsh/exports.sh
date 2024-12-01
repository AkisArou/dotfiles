if [ -f ~/.env ]; then
  export $(cat ~/.env | xargs)
fi

# Set the THEME variable to the value from ~/.env or default to "vscode"
export THEME="${THEME:-vscode}"

export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/usr/bin/go/bin:$PATH"
export PATH="/opt/asdf-vm/bin:$PATH"
export PATH="$HOME/.local/share/neovim/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

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

export TERM="xterm-256color"
export EDITOR="nvim"
export VISUAL="nvim"
export LAUNCH_EDITOR="/home/$(echo "$USER")/dotfiles/scripts/launch-editor.sh"
export BROWSER="librewolf"
export MANPAGER="nvim +Man!"
export MANWIDTH=999

export QT_SCALE_FACTOR=2
export QT_FONT_DPI=243

export FZF_DEFAULT_COMMAND="fd --type file --follow --hidden --exclude .git"

FZF_TOKYONIGHT="\
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

FZF_VSCODE="\
  --color=bg:#1f1f1f \
  --color=bg+:#222222 \
  --color=border:#1f1f1f \
  --color=fg:#d4d4d4 \
  --color=gutter:#2D2D2D \
  --color=header:#646695 \
  --color=hl:#C586C0 \
  --color=hl+:#608b4e \
  --color=info:#8db9e2 \
  --color=marker:#4EC9B0 \
  --color=pointer:#4EC9B0 \
  --color=prompt:#C586C0 \
  --color=query:#d4d4d4:regular \
  --color=scrollbar:#2d2d2d \
  --color=separator:#223e55 \
  --color=spinner:#4ec9b0 \
"

FZF_THEME="FZF_$(echo "$THEME" | tr '[:lower:]' '[:upper:]')"

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS \
  --bind ctrl-d:page-down,ctrl-u:page-up,ctrl-e:accept
  --highlight-line \
  --info=inline-right \
  --ansi \
  --layout=reverse \
  --border=none \
  $(eval echo \$"$FZF_THEME")
"

export NODE_OPTIONS="--max-old-space-size=8192 --disable-warning=ExperimentalWarning --experimental-transform-types"
export NODE_COMPILE_CACHE="$HOME/.cache/node-compile-cache"

export DESKTOP="192.168.1.2"
export LAPTOP="192.168.1.3"

# . ~/.asdf/plugins/java/set-java-home.zsh
# Instead of the above script that evaluates before each cmd, run this once.
asdf_update_java_home() {
  local java_path
  java_path="$(asdf which java)"
  if [[ -n "${java_path}" ]]; then
    export JAVA_HOME
    JAVA_HOME="$(dirname "$(dirname "${java_path:A}")")"
    export JDK_HOME=${JAVA_HOME}
  fi
}

asdf_update_java_home
