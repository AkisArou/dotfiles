#!/bin/bash

PID=$(pgrep -f $HOME/.local/share/nvim/mason/bin/vtsls)

sudo renice -10 -g $PID

notify-send "PID: $PID"
