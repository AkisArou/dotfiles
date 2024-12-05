#!/usr/bin/sh

if [ -f ~/.env ]; then
  source ~/.env
fi

foot --override include=~/dotfiles/foot/"${THEME:-vscode}".ini
