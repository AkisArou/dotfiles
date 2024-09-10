#!/usr/bin/zsh

if [[ "$HOST" == "akisarou-desktop" ]]; then
  font_size=8
else
  font_size=9
fi

alacritty --option="font.size=$font_size"
