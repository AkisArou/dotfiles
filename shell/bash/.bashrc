source "$HOME/dotfiles/shell/common/exports"
source "$HOME/dotfiles/shell/common/aliases"

[[ $- == *i* ]] && source /usr/share/blesh/ble.sh --noattach --rcfile ~/dotfiles/shell/bash/blerc

if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi

if [[ $- == *i* ]]; then # in interactive session
  set -o vi
fi

fastfetch

export STARSHIP_CONFIG=~/.config/starship/starship.toml
eval "$(starship init bash)"

shopt -s checkwinsize

# completions
. <(asdf completion bash)

[[ ! ${BLE_VERSION-} ]] || ble-attach
