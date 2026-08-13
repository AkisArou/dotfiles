source "$HOME/dotfiles/shell/common/exports"
source "$HOME/dotfiles/shell/common/aliases"

if [[ $- == *i* ]]; then # in interactive session
  set -o vi

  source "$HOME/dotfiles/shell/common/fzf"
  source "$HOME/dotfiles/shell/bash/history"

  shopt -s checkwinsize

  # completions
  . <(asdf completion bash)
fi
