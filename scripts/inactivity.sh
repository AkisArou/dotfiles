#!/usr/bin/zsh

export PRIMARY_DISPLAY="$(xrandr | awk '/ primary/{print $1}')"

if [[ "$HOST" == "archlinux-xps" ]]; then
  export SUSPEND_TIME=1800
else
  export SUSPEND_TIME=$((3600*3))
fi

xidlehook \
  --not-when-fullscreen \
  --not-when-audio \
  --timer 120 \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness .1' \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness 1' \
  --timer $SUSPEND_TIME \
    'systemctl suspend' \
    ''
