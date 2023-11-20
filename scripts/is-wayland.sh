#!/bin/sh

# Function to check if the session is Wayland
isWayland() {
	[[ -n $XDG_SESSION_TYPE && $XDG_SESSION_TYPE == 'wayland' ]]
}
