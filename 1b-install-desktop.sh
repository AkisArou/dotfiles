#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Installing desktop packages..."
echo "------------------------------"
echo ""

packages=(
  "intel-ucode"
  "radeontop"
  "mesa-vdpau"
  "mesa"
  "vulkan-radeon"
)

install_packages "${packages[@]}"
