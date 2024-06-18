#!/bin/bash
#
source "$(dirname "$0")/scripts/library.sh"

packagesYay=(
	"xorg"
	"xorg-xinit"
	"xorg-xinput"
	"i3-wm"
	"i3status"
	"i3blocks"
	"i3lock"
	"i3status-rust"
	"rofi"
	"rofi-power-menu"
	"nitrogen"
	"xclip"
	"xdg-desktop-portal-gtk"
	"xf86-video-intel"
	"polybar"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
