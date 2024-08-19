#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "xf86-video-intel"
  "intel-media-driver"
  "intel-gpu-tools"
)

install_packages "${packages[@]}"
