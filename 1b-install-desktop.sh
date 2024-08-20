#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "radeontop"
  "mesa-vdpau"
)

install_packages "${packages[@]}"
