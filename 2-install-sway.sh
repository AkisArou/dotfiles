#!/bin/bash
# ------------------------------------------------------
# Install Script for sway
# ------------------------------------------------------

# ------------------------------------------------------
# Confirm Start
# ------------------------------------------------------
source $(dirname "$0")/scripts/library.sh
clear

while true; do
	read -p "DO YOU WANT TO START THE INSTALLATION NOW? (Yy/Nn): " yn
	case $yn in
	[Yy]*)
		echo "Installation started."
		break
		;;
	[Nn]*)
		exit
		break
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
	"rofi-power-menu"
	"swaybg"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"
_installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
