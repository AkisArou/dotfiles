#!/bin/bash
#
source "$(dirname "$0")/scripts/library.sh"

packagesYay=(
  "xorg"
  "xorg-xinit"
  "xorg-xinput"
  "i3-wm"
  "i3lock"
  "xidlehook"
  "rofi"
  "rofi-power-menu"
  "nitrogen"
  "xclip"
  "xf86-video-intel"
  "polybar"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
installPackagesYay "${packagesYay[@]}"

echo "DONE! Please reboot your system!"
