#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

packagesYay=(
  "radeonsi"
  "radeontop"
)

installPackagesYay "${packagesYay[@]}"
