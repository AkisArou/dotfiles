#!/usr/bin/zsh

if [[ "$HOST" == "akisarou-desktop" ]]; then
  ~/dotfiles/st/st -s 0.9 -f "MesloLGS Nerd Font Mono:pixelsize=12:antialias=true:autohint=true"
else
  ~/dotfiles/st/st -s 0.9 -f "MesloLGS Nerd Font Mono:pixelsize=25:antialias=true:autohint=true"
fi
