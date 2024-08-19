#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packages=(
  "intel-media-driver"
  "intel-gpu-tools"
)

install_packages "${packages[@]}"
