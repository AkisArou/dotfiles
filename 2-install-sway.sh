#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "sway"
  "swaybg"
  "polkit"
  "wl-clipboard"
  "waybar"
  "tofi"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"

echo "DONE! Please reboot your system!"
