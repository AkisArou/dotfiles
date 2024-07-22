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
	"usbutils"
	"tldr"
	"bleachbit"
	"wget"
	"curl"
	"yazi"
	"htop"
	"firewalld"
	"openbsd-netcat"
	"acpi"
	"acpi_call"
	"rsync"
	"pipewire"
	"pipewire-alsa"
	"pipewire-audio"
	"pipewire-pulse"
	"pipewire-jack"
	"dnsutils"
	"inetutils"
	"xdg-user-dirs"
	"xdg-utils"
	"avahi"
	"dialog"
	"wpa_supplicant"
	"iwd"
	"iwgtk"
	"openssh"
	"pacman-contrib"
	"alacritty"
	"chromium"
	"dunst"
	"freerdp"
	"pcmanfm-gtk3"
	"ttf-hack-nerd"
	"ttf-font-awesome"
	"noto-fonts-emoji"
	"terminus-font"
	"figlet"
	"vlc"
	"exa"
	"python-pip"
	"xdg-desktop-portal-gtk"
	"pavucontrol"
	"tumbler"
	"blueman"
	"bluez"
	"bluez-utils"
	"bat"
	"zsh"
	"brightnessctl"
	"zip"
	"unzip"
	"brave"
	"fastfetch"
	"trizen"
	"base-devel"
	"ripgrep"
	"fd"
	"snapd"
	"neovim"
	"visual-studio-code-bin"
	"caprine"
	"tmux"
	"linux-headers"
	"genymotion"
	"git-delta"
	"gnome-terminal"
	"docker-desktop"
	"docker-compose"
	"jetbrains-toolbox"
	"fzf"
	"jq"
	"watchman-bin"
	"microsoft-edge-stable-bin"
	"rar"
	"tod0"
	"wireplumber"
	"udisks2"
	"fwupd"
	"ueberzugpp"
	"imagemagick"
	"ttf-meslo-nerd"
  "preload"
  "pnpm-shell-completion"
  "wol"
  "ghostscript"
  "libreoffice-still"
  "kitty"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesYay "${packagesYay[@]}"
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
usermod -aG wheel $username

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
clear
echo "DONE!"
