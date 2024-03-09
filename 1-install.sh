#!/bin/bash
# ------------------------------------------------------
# Load Library
# ------------------------------------------------------
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
# Confirm Start
# ------------------------------------------------------

while true; do
	read -p -r "DO YOU WANT TO START THE INSTALLATION NOW? (Yy/Nn): " yn
	case $yn in
	[Yy]*)
		echo "Installation started."
		break
		;;
	[Nn]*)
		exit
		;;
	*) echo "Please answer yes or no." ;;
	esac
done

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
echo ""
echo "-> Install main packages"

packagesPacman=(
	"pacman-contrib"
	"alacritty"
	"chromium"
	"dunst"
	"freerdp"
	"pcmanfm-gtk3"
	"ttf-hack-nerd"
	"ttf-font-awesome"
	"ttf-nerd-fonts-input"
	"figlet"
	"vlc"
	"exa"
	"python-pip"
	"xdg-desktop-portal-gtk"
	"pavucontrol"
	"tumbler"
	"blueman"
	"bat"
	"zsh"
)

packagesYay=(
	"brave-bin"
	"pfetch"
	"trizen"
	"base-devel"
	"ripgrep"
	"fd"
	"snapd"
	"neovim"
	"visual-studio-code-bin"
	"caprine"
	"tmux"
	"waybar"
	"virtualbox"
	"linux-lts-headers"
	"genymotion"
	"lazygit"
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
	"github-cli"
	"wireplumber"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"
_installPackagesYay "${packagesYay[@]}"
zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1

# ------------------------------------------------------
# Install custom issue (login prompt)
# ------------------------------------------------------
echo ""
echo "-> Install login screen"
while true; do
	read -p -r "Do you want to install the custom login prompt? (Yy/Nn): " yn
	case $yn in
	[Yy]*)
		sudo cp ~/dotfiles/login/issue /etc/issue
		echo "Login prompt installed."
		break
		;;
	[Nn]*)
		echo "Custom login prompt skipped."
		break
		;;
	*) echo "Please answer yes or no." ;;
	esac
done

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
clear
echo "DONE!"
