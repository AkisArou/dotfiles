#!/usr/bin/zsh

export PRIMARY_DISPLAY="$(xrandr | awk '/ primary/{print $1}')"

xidlehook \
  --not-when-fullscreen \
  --not-when-audio \
  --timer 120 \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness .1' \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness 1'
