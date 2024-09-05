#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "intel-ucode"
  "radeontop"
  "mesa-vdpau"
)

install_packages "${packages[@]}"
