#!/bin/zsh

# Load all completions from ~/.completions
if [[ -d "$HOME/.completions" ]]; then
  for file in "$HOME/.completions"/*; do
    [[ -f "$file" ]] && source "$file"
  done
fi
