#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "intel-ucode"
  "radeontop"
  "mesa-vdpau"
  "libva-mesa-driver"
)

install_packages "${packages[@]}"
