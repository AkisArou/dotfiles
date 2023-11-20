#!/bin/sh
#   ___ _____ ___ _     _____   ____  _             _
#  / _ \_   _|_ _| |   | ____| / ___|| |_ __ _ _ __| |_
# | | | || |  | || |   |  _|   \___ \| __/ _` | '__| __|
# | |_| || |  | || |___| |___   ___) | || (_| | |  | |_
#  \__\_\|_| |___|_____|_____| |____/ \__\__,_|_|   \__|
#
# -----------------------------------------------------

# Screen resolution
# -- PC
xrandr --output DisplayPort-1 --mode 5120x1440 --rate 120
# -- Dell XPS
# TODO

# Set keyboard mapping
setxkbmap en

# Load power manager
xfce4-power-manager &

# Load notification service
dunst &
