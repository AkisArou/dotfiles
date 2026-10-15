source "$HOME/.config/shell/common/exports"
source "$HOME/.config/shell/common/aliases"

if [[ $- == *i* ]]; then # in interactive session
  set -o vi

  source "$HOME/.config/shell/common/fzf"
  source "$HOME/.config/shell/bash/history"

  shopt -s checkwinsize

  # completions
  . <(asdf completion bash)
fi
