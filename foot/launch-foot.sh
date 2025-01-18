#!/usr/bin/sh

if [ -f ~/.env ]; then
  source ~/.env
fi

# foot --override include=~/dotfiles/foot/"${THEME:-vscode}".ini
foot --override include=~/dotfiles/foot/onehalf.ini
