#!/usr/bin/zsh

if [[ "$HOST" == "akisarou-desktop" ]]; then
  rofi -show drun
else
  rofi -show drun -dpi 180
fi
