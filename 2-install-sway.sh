#!/bin/bash
# ------------------------------------------------------
# Install Script for sway
# ------------------------------------------------------

# ------------------------------------------------------
# Confirm Start
# ------------------------------------------------------
source $(dirname "$0")/scripts/library.sh
clear

echo "  ___ _____ ___ _     _____  "
echo "  _____      ____ _ _   _ "
echo ""/ __\ \ /\ / / _` | | | |""
echo ""\__ \\ V  V / (_| | |_| |""
echo " |___/ \_/\_/ \__,_|\__, |"
echo "                     __/ |"
echo "                    |___/ "
echo ""

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
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"

echo "DONE! Please reboot your system!"
