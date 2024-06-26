#!/usr/bin/zsh

FONT_FAMILY="MesloLGS Nerd Font Mono"

if [[ "$HOST" == "akisarou-desktop" ]]; then
	export POLY_FONT="$FONT_FAMILY:size=10"
	export POLY_BAR_HEIGHT="20pt"
else
	export POLY_FONT="$FONT_FAMILY:size=20"
	export POLY_BAR_HEIGHT="30pt"
fi

polybar default &
