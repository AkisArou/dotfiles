#!/usr/bin/zsh

if [[ "$HOST" == "arch-desktop" ]]; then
  rofi -show power-menu -modi power-menu:rofi-power-menu
else
  rofi -show power-menu -modi power-menu:rofi-power-menu -dpi 180
fi
