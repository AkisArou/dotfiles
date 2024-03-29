#!/bin/bash
#
packagesPacman=(
)

packagesYay=(
	"xorg-xinput"
	"rofi"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"
_installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
