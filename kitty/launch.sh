#!/usr/bin/zsh

if [[ "$HOST" == "arch-desktop" ]]; then
  font_size=8
else
  font_size=10
fi

kitty -o "font.size=$font_size"
