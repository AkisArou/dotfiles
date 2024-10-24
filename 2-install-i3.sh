#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "xorg"
  "xorg-xinit"
  "xorg-xinput"
  "i3-wm"
  "xidlehook"
  "rofi"
  "rofi-power-menu"
  "nitrogen"
  "xclip"
  "polybar"
  "flameshot"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"

echo "DONE! Please reboot your system!"
