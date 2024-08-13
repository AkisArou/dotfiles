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
# Check if yay is installed
# ------------------------------------------------------
if sudo pacman -Qs yay >/dev/null; then
  echo "yay is installed. You can proceed with the installation"
else
  echo "yay is not installed. Will be installed now!"
  git clone https://aur.archlinux.org/yay-git.git ~/yay-git
  cd ~/yay-git || exit
  makepkg -si
  cd ~/dotfiles/ || return
  clear
  echo "yay has been installed successfully."
  echo ""
  echo "  ___           _        _ _  "
  echo " |_ _|_ __  ___| |_ __ _| | | "
  echo "  | ||  _ \/ __| __/ _  | | | "
  echo "  | || | | \__ \ || (_| | | | "
  echo " |___|_| |_|___/\__\__,_|_|_| "
  echo "                              "
  echo "-------------------------------------"
  echo ""
fi

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
echo ""
echo "-> Install main packages"

# ------------------------------------------------------
# Install Packages
# ------------------------------------------------------
packagesYay=(
  "fwupd"
  "linux-headers"
  "base-devel"
  "acpi"
  "acpid"
  "acpi_call"
  "firewalld"
  "wol"
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
  "wireplumber"
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
  "chromium"
  "microsoft-edge-stable-bin"
  "firefox"
  "dunst"
  "pcmanfm-gtk3"
  "ttf-hack-nerd"
  "ttf-font-awesome"
  "ttf-meslo-nerd"
  "noto-fonts-emoji"
  "terminus-font"
  "exa"
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
)

# ------------------------------------------------------
# ST
# ------------------------------------------------------
sudo make -C ~/dotfiles/st clean install

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
installPackagesYay "${packagesYay[@]}"
zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1

# ------------------------------------------------------
# Install custom issue (login prompt)
# ------------------------------------------------------
echo ""
echo "-> Install login screen"
sudo cp ~/dotfiles/login/issue /etc/issue
echo "Login prompt installed."

# ------------------------------------------------------
# Add user to wheel
# ------------------------------------------------------
clear
echo "Uncomment %wheel group in sudoers (around line 85):"
echo "Before: #%wheel ALL=(ALL:ALL) ALL"
echo "After:  %wheel ALL=(ALL:ALL) ALL"
echo ""
read -p "Open sudoers now?" c
EDITOR=vim sudo -E visudo
usermod -aG wheel "$username"

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
clear
echo "DONE!"
