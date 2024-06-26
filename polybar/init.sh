#!/usr/bin/zsh

if [[ "$HOST" == "akisarou-desktop" ]]; then
	export POLY_FONT="MesloLGS Nerd Font Mono:size=10"
	export POLY_BAR_HEIGHT="20pt"
else
	export POLY_FONT="MesloLGS Nerd Font Mono:size=20"
	export POLY_BAR_HEIGHT="30pt"
fi

polybar default &
