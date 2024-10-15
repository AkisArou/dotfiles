#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

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
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"

echo "DONE! Please reboot your system!"
