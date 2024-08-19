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
fi

# ------------------------------------------------------
# Install Packages
# ------------------------------------------------------
packagesYay=(
  "fwupd"
  "udisks2" # Needed for fwupd
  "linux-headers"
  "base-devel"
  "posix"
  "acpi"
  "firewalld"
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
  "librewolf-bin"
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
# Install required packages
# ------------------------------------------------------
installPackagesYay "${packagesYay[@]}"
zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1

# ------------------------------------------------------
# ST
# ------------------------------------------------------
sudo make -C ~/dotfiles/st clean install

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
echo "Uncommenting %wheel group in sudoers..."

sudo sed -i.bak 's/^#\s*\(%wheel\s\+ALL=(ALL:ALL)\s\+ALL\)/\1/' /etc/sudoers

if grep -q '^%wheel ALL=(ALL:ALL) ALL' /etc/sudoers; then
  echo "Success: %wheel group has been uncommented in sudoers."
else
  echo "Error: Failed to uncomment %wheel group in sudoers."
fi

sudo usermod -aG wheel "$USER"

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
echo "DONE!"
