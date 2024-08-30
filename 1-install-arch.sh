#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "  ___           _        _ _  "
echo " |_ _|_ __  ___| |_ __ _| | | "
echo "  | ||  _ \/ __| __/ _  | | | "
echo "  | || | | \__ \ || (_| | | | "
echo " |___|_| |_|___/\__\__,_|_|_| "
echo "                              "
echo "-------------------------------------"
echo ""

# ------------------------------------------------------
# Paru
# ------------------------------------------------------
if sudo pacman -Qs paru >/dev/null; then
  echo "paru is installed..."
else
  echo "paru is not installed. Will be installed now!"
  git clone https://aur.archlinux.org/paru.git ~/paru
  cd ~/paru || exit
  makepkg -si
  cd ~/dotfiles/ || return
  clear
  rm -rf ~/paru
  echo "paru has been installed successfully."
fi

# ------------------------------------------------------
# Install Packages
# ------------------------------------------------------
packages=(
  "mesa"
  "audit"
  "git"
  "fwupd"
  "udisks2" # Needed for fwupd
  "linux-headers"
  "base"
  "base-devel"
  "posix"
  "acpi"
  "wol"
  "libva-utils"
  "libvdpau-va-gl"
  "vdpauinfo"
  "rsync"
  "preload"
  "wget"
  "curl"
  "htop"
  "tldr"
  "bleachbit"
  "yazi"
  "openbsd-netcat"
  "pipewire"
  "pipewire-alsa"
  "pipewire-audio"
  "pipewire-pulse"
  "pipewire-jack"
  "xdg-user-dirs"
  "xdg-utils"
  "avahi"
  "wpa_supplicant"
  "iwd"
  "openssh"
  "vlc"
  "zsh"
  "alacritty"
  "kitty"
  "brave-bin"
  "librewolf-bin"
  "dunst"
  "pcmanfm-gtk3"
  "ttf-hack-nerd"
  "ttf-font-awesome"
  "ttf-meslo-nerd"
  "noto-fonts-emoji"
  "terminus-font"
  "eza"
  "bat"
  "python-pip"
  "xdg-desktop-portal-gtk"
  "pavucontrol"
  "bluez"
  "bluez-utils"
  "brightnessctl"
  "zip"
  "unzip"
  "rar"
  "flameshot"
  "fastfetch"
  "ripgrep"
  "fd"
  "neovim"
  "vim"
  "visual-studio-code-bin"
  "tmux"
  "genymotion"
  "git-delta"
  "gnome-terminal"
  "docker"
  "docker-compose"
  "jetbrains-toolbox"
  "fzf"
  "jq"
  "ueberzugpp"
  "imagemagick"
  "ghostscript"
  "pnpm-shell-completion"
  "libreoffice-still"
  "android-studio"
  "zed"
  "watchman-bin"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
install_packages "${packages[@]}"
zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1

# ------------------------------------------------------
# ST
# ------------------------------------------------------
echo "Making st..."
sudo make -C ~/dotfiles/st clean install

# ------------------------------------------------------
# Install custom issue (login prompt)
# ------------------------------------------------------
echo ""
echo "Installing login screen..."
sudo cp ~/dotfiles/login/issue /etc/issue
echo "Login prompt installed."

# ------------------------------------------------------
# Add user to wheel
# ------------------------------------------------------
sudoers_line='%wheel ALL=(ALL:ALL) ALL'

if sudo grep -q "^${sudoers_line}$" /etc/sudoers; then
  echo "The %wheel group is already uncommented in sudoers."
else
  echo "Uncommenting %wheel group in sudoers..."

  sudo sed -i.bak 's/^#\s*\(%wheel\s\+ALL=(ALL:ALL)\s\+ALL\)/\1/' /etc/sudoers

  if sudo grep -q "^${sudoers_line}$" /etc/sudoers; then
    echo "Success: %wheel group has been uncommented in sudoers."
  else
    echo "Error: Failed to uncomment %wheel group in sudoers."
  fi
fi

sudo usermod -aG wheel "$USER"

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
echo "DONE! set zsh as your default shell if not yet"
