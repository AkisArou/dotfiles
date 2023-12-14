#!/bin/bash

if (swaymsg --type get_outputs | grep 'DP-2'); then
	swaymsg "smart_gaps inverse_outer"
	swaymsg "gaps horizontal 100"
fi
