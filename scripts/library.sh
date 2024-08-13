#!/bin/bash

islnstalledYay() {
  package="$1"
  check="$(yay -Qs --color always "${package}" | grep "local" | grep "${package} ")"
  if [ -n "${check}" ]; then
    echo 0
    return
  fi
  echo 1
  return
}

installPackagesYay() {
  toInstall=()

  for pkg; do
    if [[ $(islnstalledYay "${pkg}") == 0 ]]; then
      echo "${pkg} is already installed."
      continue
    fi

    toInstall+=("${pkg}")
  done

  if [[ "${toInstall[@]}" == "" ]]; then
    return
  fi

  printf "Packages to install:\n%s\n" "${toInstall[@]}"
  yay --noconfirm -S "${toInstall[@]}"
}

create_symlink() {
  local target=$1
  local link_name=$2

  # Check if the target exists
  if [[ ! -e $target ]]; then
    echo "Error: Target '$target' does not exist."
    return 1
  fi

  # Check if the link name already exists
  if [[ -e $link_name || -L $link_name ]]; then
    if [[ -L $link_name ]]; then
      # It's a symlink, check if it points to the same target
      existing_target=$(readlink "$link_name")
      if [[ $existing_target == $target ]]; then
        echo "Symlink '$link_name' already exists and points to '$target'."
        return 0
      else
        echo "Warning: Symlink '$link_name' points to a different target '$existing_target'."
        read -p "Do you want to replace it? (y/n) " choice
        if [[ $choice != "y" ]]; then
          echo "Skipping '$link_name'."
          return 1
        fi
        # Remove the old symlink
        rm -f "$link_name"
      fi
    else
      # It's not a symlink, back it up or remove it
      echo "Warning: '$link_name' already exists and is not a symlink."
      read -p "Do you want to back it up and replace it? (y/n) " choice
      if [[ $choice != "y" ]]; then
        echo "Skipping '$link_name'."
        return 1
      fi
      mv "$link_name" "${link_name}.bak"
      echo "Backed up '$link_name' to '${link_name}.bak'."
    fi
  fi

  # Create the symlink
  ln -s "$target" "$link_name"
  echo "Created symlink: '$link_name' -> '$target'."
}
