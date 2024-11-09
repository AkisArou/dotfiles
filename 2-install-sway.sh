#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Installing wayland with sway wm..."
echo "----------------------------------"
echo ""

packages=(
  "sway"
  "swaybg"
  "swayidle"
  "swaydim"
  "polkit"
  "wl-clipboard"
  "waybar"
  "tofi"
  "grim"
  "grimshot"
  "xdg-desktop-portal-wlr"
  "xorg-xwayland"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"

echo "DONE! Please reboot your system!"
