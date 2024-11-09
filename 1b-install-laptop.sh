#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Installing laptop packages..."
echo "-----------------------------"
echo ""

packages=(
  "intel-ucode"
  "intel-media-driver"
  "intel-gpu-tools"
)

install_packages "${packages[@]}"
