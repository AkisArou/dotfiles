#!/usr/bin/zsh

FONT_FAMILY="MesloLGS Nerd Font Mono"

if [[ "$HOST" == "akisarou-desktop" ]]; then
  POLY_FONT="$FONT_FAMILY":size=10
  POLY_BAR_HEIGHT="20pt"
else
  POLY_FONT="$FONT_FAMILY":size=20
  POLY_BAR_HEIGHT="30pt"
fi

export POLY_FONT
export POLY_BAR_HEIGHT

polybar default &
