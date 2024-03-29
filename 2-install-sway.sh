#!/bin/bash
# ------------------------------------------------------
# Install Script for sway
# ------------------------------------------------------

# ------------------------------------------------------
# Confirm Start
# ------------------------------------------------------
source "$(dirname "$0")/scripts/library.sh"
clear

while true; do
	read -p -r "DO YOU WANT TO START THE INSTALLATION NOW? (Yy/Nn): " yn
	case $yn in
	[Yy]*)
		echo "Installation started."
		break
		;;
	[Nn]*)
		exit
		;;
	*) echo "Please answer yes or no." ;;
	esac
done
echo ""

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
echo ""
echo "-> Install main packages"

packagesPacman=(
	"sway"
	"swaylock"
	"rofi"
)

packagesYay=(
	"swayidle"
	"rofi-power-menu"
	"swaybg"
	"wlsunset"
	"wl-clipboard"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"
_installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
