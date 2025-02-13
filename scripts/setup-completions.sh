#!/bin/zsh

COMPLETIONS_DIR="$HOME/.completions"
mkdir -p "$COMPLETIONS_DIR"

NPM_COMPLETION="$COMPLETIONS_DIR/_npm"
if [[ ! -f "$NPM_COMPLETION" ]]; then
  npm completion >"$NPM_COMPLETION"
fi

DOCKER_COMPLETION="$COMPLETIONS_DIR/_docker"
if [[ ! -f "$DOCKER_COMPLETION" ]]; then
  docker completion zsh >"$DOCKER_COMPLETION"
fi

GH_COMPLETION="$COMPLETIONS_DIR/_gh"
if [[ ! -f "$GH_COMPLETION" ]]; then
  gh completion -s zsh >"$GH_COMPLETION"
fi
