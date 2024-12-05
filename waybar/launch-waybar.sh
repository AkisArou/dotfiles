#!/usr/bin/zsh

if [ -f ~/.env ]; then
  source ~/.env
fi

waybar --style ~/dotfiles/waybar/"${THEME:-vscode}".css
