#!/bin/bash

is_package_installed() {
  package="$1"
  check="$(yay -Qs --color always "${package}" | grep "local" | grep "${package} ")"
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

find_extra_packages() {
  local -n packages_in_list="$1"

  # Read explicitly installed packages into an array
  IFS=$'\n' read -r -d '' -a explicit_packages < <(yay -Qe | awk '{print $1}' && printf '\0')

  local missing_packages=()
  local extra_packages=()
  #
  # Check each explicitly installed package against the provided list
  for item2 in "${explicit_packages[@]}"; do
    local found=false

    for item1 in "${packages_in_list[@]}"; do
      if [[ "$item2" == "$item1" ]]; then
        found=true
        break
      fi
    done

    if [[ "$found" == false ]]; then
      extra_packages+=("$item2")
    fi
  done

  # Output the extra packages
  if [ ${#extra_packages[@]} -eq 0 ]; then
    echo "No extra explicitly installed packages found."
  else
    echo "Explicitly installed packages not in the provided list:"
    for package in "${extra_packages[@]}"; do
      echo "$package"
    done
  fi
}

find_removed_packages() {
  local -n packages_in_list="$1"

  # Read explicitly installed packages into an array
  IFS=$'\n' read -r -d '' -a explicit_packages < <(yay -Qe | awk '{print $1}' && printf '\0')

  local missing_packages=()
  local extra_packages=()

  # Check each package in the provided list
  for item1 in "${packages_in_list[@]}"; do
    local found=false

    for item2 in "${explicit_packages[@]}"; do
      if [[ "$item1" == "$item2" ]]; then
        found=true
        break
      fi
    done

    if [[ "$found" == false ]]; then
      missing_packages+=("$item1")
    fi
  done

  # Output the missing packages
  if [ ${#missing_packages[@]} -eq 0 ]; then
    echo "All explicitly installed packages are in the provided list."
  else
    echo "Packages in the provided list but not installed explicitly:"
    for package in "${missing_packages[@]}"; do
      echo "$package"
    done
  fi

}
