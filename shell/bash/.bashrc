source "$HOME/dotfiles/shell/common/exports"
source "$HOME/dotfiles/shell/common/aliases"

if [[ $- == *i* ]]; then # in interactive session
  set -o vi
fi

shopt -s checkwinsize

# completions
. <(asdf completion bash)
