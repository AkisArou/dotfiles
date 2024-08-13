#!/usr/bin/zsh

if [[ "$HOST" == "akisarou-desktop" ]]; then
  ~/dotfiles/st/st -f "MesloLGS Nerd Font Mono:pixelsize=15:antialias=true:autohint=true"
else
  ~/dotfiles/st/st -f "MesloLGS Nerd Font Mono:pixelsize=25:antialias=true:autohint=true"
fi
