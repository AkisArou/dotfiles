#!/bin/bash

# Get the output of swaymsg --type get_outputs and filter for 'DP-2'
output=$(swaymsg --type get_outputs | grep 'DP-4')

# Check if the output is empty
if [ -n "$output" ]; then
	# If not empty, run the specified swaymsg commands
	swaymsg "smart_gaps inverse_outer"
	swaymsg "gaps horizontal 1500"
fi
