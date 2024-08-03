#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packagesYay=(
  "intel-media-driver"
  "intel-gpu-tools"
  "libva-utils"
  "vdpauinfo"
  "libvdpau-va-gl"
)

_installPackagesYay "${packagesYay[@]}"
