#!/bin/bash
#
packagesPacman=(
)

packagesYay=(
	"xorg-xinput"
	"rofi"
	"rofi-power-menu"
	"nitrogen"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"
_installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
