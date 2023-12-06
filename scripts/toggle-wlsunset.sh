#!/usr/bin/bash

if pidof wlsunset; then
	killall -9 wlsunset
else
	wlsunset -l 37.9 -L 23.7
fi
