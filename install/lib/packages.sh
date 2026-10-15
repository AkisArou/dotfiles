#!/bin/bash

source "$HOME/dotfiles/home/.config/shell/common/functions"

is_package_installed() {
  package="$1"
  check="$(paru -Qs --color always "${package}" | grep "local" | grep "${package} ")"
  if [ -n "${check}" ]; then
    echo 0
    return
  fi
  echo 1
  return
}

install_packages() {
  toInstall=()

  for pkg; do
    if [[ $(is_package_installed "${pkg}") == 0 ]]; then
      print_success "${pkg} is already installed."
      continue
    fi

    toInstall+=("${pkg}")
  done

  if [[ "${toInstall[@]}" == "" ]]; then
    return
  fi

  printf "Packages to install:\n%s\n" "${toInstall[@]}"
  paru --noconfirm -S "${toInstall[@]}"
}
