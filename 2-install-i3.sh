#!/bin/bash
#
source "$(dirname "$0")/scripts/library.sh"

packages=(
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
  "polybar"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"

echo "DONE! Please reboot your system!"
