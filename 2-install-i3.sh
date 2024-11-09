#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Installing xorg with i3wm..."
echo "----------------------------"
echo ""

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

print_success "DONE!"
