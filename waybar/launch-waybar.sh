#!/usr/bin/zsh

if [ -f ~/.env ]; then
  source ~/.env
fi

killall waybar
waybar --style ~/dotfiles/waybar/"${THEME:-onedark}".css
