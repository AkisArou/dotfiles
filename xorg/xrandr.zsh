#!/bin/zsh

# Get the name of the connected output (assuming only one output is connected)
output=$(xrandr | grep " connected" | awk '{ print $1 }')

# Get the list of resolutions for the connected output
resolutions=$(xrandr | awk '{ print $1 }')

# Find the highest resolution
highest_resolution=$(echo "$resolutions" | sort -nr | head -n1)

# Apply the highest resolution
xrandr --output $output --mode $highest_resolution
