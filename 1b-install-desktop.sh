#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "xf86-video-amdgpu"
  "radeonsi"
  "radeontop"
  "mesa-vdpau"
)

install_packages "${packages[@]}"
