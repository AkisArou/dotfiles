#!/bin/bash

# ------------------------------------------------------
# Load Library
# ------------------------------------------------------
source "$(dirname "$0")/scripts/library.sh"
clear
echo "     _       _    __ _ _            "
echo "  __| | ___ | |_ / _(_) | ___  ___  "
echo " / _' |/ _ \| __| |_| | |/ _ \/ __| "
echo "| (_| | (_) | |_|  _| | |  __/\__ \ "
echo " \__,_|\___/ \__|_| |_|_|\___||___/ "
echo "                                    "
echo "-------------------------------------"
echo ""
echo "The script will ask for permission to remove existing folders and files."
echo "But you can decide to keep your local versions by answering with No (Nn)."
echo "Symbolic links will be created from ~/dotfiles into your home and .config directories."
echo ""

# ------------------------------------------------------
# Create .config folder
# ------------------------------------------------------
echo ""
echo "-> Check if .config folder exists"

if [ -d ~/.config ]; then
	echo ".config folder already exists."
else
	mkdir ~/.config
	echo ".config folder created."
fi

# ------------------------------------------------------
# Create symbolic links
# ------------------------------------------------------
# name symlink source target

echo ""
echo "-------------------------------------"
echo "-> Install general dotfiles"
echo "-------------------------------------"
echo ""

_installSymLink alacritty ~/.config/alacritty ~/dotfiles/alacritty/ ~/.config
_installSymLink ranger ~/.config/ranger ~/dotfiles/ranger/ ~/.config
_installSymLink nvim ~/.config/nvim ~/dotfiles/nvim/ ~/.config
_installSymLink .vimrc ~/.config/.vimrc ~/dotfiles/vim/.vimrc ~/.vimrc
_installSymLink vim ~/.config/.vim ~/dotfiles/vim/ ~/.vim
_installSymLink rofi ~/.config/rofi ~/dotfiles/rofi/ ~/.config
_installSymLink dunst ~/.config/dunst ~/dotfiles/dunst/ ~/.config
_installSymLink ssh ~/.ssh/config ~/dotfiles/ssh/config ~/.ssh/config

echo ""
echo "-------------------------------------"
echo "-> Install X11 dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink xorg.conf /etc/X11/xorg.conf ~/dotfiles/xorg/xorg.conf /etc/X11/xorg.conf
_installSymLink .xinitrc ~/.xinitrc ~/dotfiles/xorg/.xinitrc ~/.xinitrc
_installSymLink .zprofile ~/.zprofile ~/dotfiles/zsh/.config/zsh/.zprofile ~/.zprofile
_installSymLink libinput-gestures ~/.config/libinput-gestures.conf ~/dotfiles/libinput-gestures.conf ~/.config/libinput-gestures.conf

echo ""
echo "-------------------------------------"
echo "-> Install GTK dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink gtk ~/.config/gtk-2.0/ ~/dotfiles/gtk/.gtkrc-2.0/ ~/.gtkrc-2.0
_installSymLink gtk-2.0 ~/.config/gtk-2.0/ ~/dotfiles/gtk/.gtkrc-2.0/ ~/.gtkrc-2.0
_installSymLink gtk-3.0 ~/.config/gtk-3.0/ ~/dotfiles/gtk/gtk-3.0/ ~/.config/
_installSymLink gtk-4.0 ~/.config/gtk-4.0/ ~/dotfiles/gtk/gtk-4.0/ ~/.config/
_installSymLink .Xresouces ~/.Xresources ~/dotfiles/gtk/.Xresources ~/.Xresources
# # For sway
# _installSymLink .Xdefaults ~/.Xresources ~/dotfiles/gtk/.Xresources ~/.Xdefaults

echo "-------------------------------------"
echo "-> Install i3 dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink i3 ~/.config/i3 ~/dotfiles/i3/ ~/.config/i3
_installSymLink i3status-rust ~/.config/i3status-rust ~/dotfiles/i3status-rust ~/.config/i3status-rust

echo "-------------------------------------"
echo "-> Install Sway dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink sway ~/.config/sway ~/dotfiles/sway/ ~/.config

echo "-------------------------------------"
echo "-> Install Swaylock dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink swaylock ~/.config/swaylock ~/dotfiles/swaylock/ ~/.config

echo "-------------------------------------"
echo "-> Install Waybar dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink waybar ~/.config/waybar ~/dotfiles/waybar/ ~/.config

echo "-------------------------------------"
echo "-> Install Nvim config"
echo "-------------------------------------"
echo ""
_installSymLink nvim ~/.config/nvim ~/dotfiles/nvim/ ~/.config

echo "-------------------------------------"
echo "-> Install pcmanfm config"
echo "-------------------------------------"
echo ""
_installSymLink pcmanfm ~/.config/pcmanfm ~/dotfiles/pcmanfm/ ~/.config

echo "-------------------------------------"
echo "-> Install Git config"
echo "-------------------------------------"
echo ""
_installSymLink git ~/.config/git ~/dotfiles/git/ ~/.config

echo "-------------------------------------"
echo "-> Install Lazygit config"
echo "-------------------------------------"
echo ""
_installSymLink lazygit ~/.config/lazygit ~/dotfiles/lazygit/ ~/.config

echo "-------------------------------------"
echo "-> Install zsh config"
echo "-------------------------------------"
echo ""
_installSymLink .zshrc ~/.zshrc ~/dotfiles/zsh/.zshrc ~/.zshrc
_installSymLink zsh-aliases ~/.config/zsh/aliases.zsh ~/dotfiles/zsh/.config/zsh/aliases.zsh
_installSymLink zsh-exports ~/.config/zsh/exports.zsh ~/dotfiles/zsh/.config/zsh/exports.zsh
_installSymLink zsh-functions ~/.config/zsh/functions.zsh ~/dotfiles/zsh/.config/zsh/functions.zsh

echo "-------------------------------------"
echo "-> Install .ideavimrc config"
echo "-------------------------------------"
echo ""
_installSymLink .ideavimrc ~/.ideavimrc ~/dotfiles/.ideavimrc ~/.ideavimrc

echo "-------------------------------------"
echo "-> Install tmux config"
echo "-------------------------------------"
echo ""
_installSymLink .tmux.conf ~/.tmux.conf ~/dotfiles/.tmux.conf ~/.tmux.conf

echo "-------------------------------------"
echo "-> Install asdf tool-versions"
echo "-------------------------------------"
echo ""
_installSymLink .tool-versions ~/.tool-versions ~/dotfiles/.tool-versions ~/.tool-versions

echo "-------------------------------------"
echo "->  Brave/Chromium configs "
echo "-------------------------------------"
echo ""
_installSymLink brave-flags ~/.config/brave-flags.conf ~/dotfiles/brave-flags.conf ~/.config
_installSymLink chromium-flags ~/.config/chromium-flags.conf ~/dotfiles/chromium-flags.conf ~/.config

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
echo "DONE! Please reboot your system and run 4-config.sh!"
