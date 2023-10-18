#!/bin/bash
#      _       _    __ _ _           
#   __| | ___ | |_ / _(_) | ___  ___ 
#  / _` |/ _ \| __| |_| | |/ _ \/ __|
# | (_| | (_) | |_|  _| | |  __/\__ \
#  \__,_|\___/ \__|_| |_|_|\___||___/
#                                    
# by Stephan Raabe (2023)
# ------------------------------------------------------
# Install Script for dotfiles and configuration
# yay must be installed
# ------------------------------------------------------

# ------------------------------------------------------
# Load Library
# ------------------------------------------------------
source $(dirname "$0")/scripts/library.sh
clear
echo "     _       _    __ _ _            "
echo "  __| | ___ | |_ / _(_) | ___  ___  "
echo " / _' |/ _ \| __| |_| | |/ _ \/ __| "
echo "| (_| | (_) | |_|  _| | |  __/\__ \ "
echo " \__,_|\___/ \__|_| |_|_|\___||___/ "
echo "                                    "
echo "by Stephan Raabe (2023)"
echo "-------------------------------------"
echo ""
echo "The script will ask for permission to remove existing folders and files."
echo "But you can decide to keep your local versions by answering with No (Nn)."
echo "Symbolic links will be created from ~/dotfiles into your home and .config directories."
echo ""

# ------------------------------------------------------
# Confirm Start
# ------------------------------------------------------
while true; do
    read -p "DO YOU WANT TO START THE INSTALLATION NOW? (Yy/Nn): " yn
    case $yn in
        [Yy]* )
            echo "Installation started."
        break;;
        [Nn]* ) 
            exit;
        break;;
        * ) echo "Please answer yes or no.";;
    esac
done

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
_installSymLink rofi ~/.config/rofi ~/dotfiles/rofi/ ~/.config
_installSymLink dunst ~/.config/dunst ~/dotfiles/dunst/ ~/.config
_installSymLink wal ~/.config/wal ~/dotfiles/wal/ ~/.config
wal -i screenshots/
echo "Pywal templates initiated!"
echo ""
echo "-------------------------------------"
echo "-> Install GTK dotfiles"
echo "-------------------------------------"
echo ""

_installSymLink .gtkrc-2.0 ~/.gtkrc-2.0 ~/dotfiles/gtk/.gtkrc-2.0 ~/.gtkrc-2.0
_installSymLink gtk-3.0 ~/.config/gtk-3.0 ~/dotfiles/gtk/gtk-3.0/ ~/.config/
_installSymLink .Xresouces ~/.Xresources ~/dotfiles/gtk/.Xresources ~/.Xresources
_installSymLink .icons ~/.icons ~/dotfiles/gtk/.icons/ ~/

echo "-------------------------------------"
echo "-> Install Qtile dotfiles"
echo "-------------------------------------"
echo ""

_installSymLink qtile ~/.config/qtile ~/dotfiles/qtile/ ~/.config
_installSymLink polybar ~/.config/polybar ~/dotfiles/polybar/ ~/.config
_installSymLink picom ~/.config/picom ~/dotfiles/picom/ ~/.config
_installSymLink .xinitrc ~/.xinitrc ~/dotfiles/qtile/.xinitrc ~/.xinitrc

echo "-------------------------------------"
echo "-> Install Hyprland dotfiles"
echo "-------------------------------------"
echo ""

_installSymLink hypr ~/.config/hypr ~/dotfiles/hypr/ ~/.config
_installSymLink waybar ~/.config/waybar ~/dotfiles/waybar/ ~/.config
_installSymLink swaylock ~/.config/swaylock ~/dotfiles/swaylock/ ~/.config
_installSymLink wlogout ~/.config/wlogout ~/dotfiles/wlogout/ ~/.config
_installSymLink swappy ~/.config/swappy ~/dotfiles/swappy/ ~/.config

echo "-------------------------------------"
echo "-> Install Neovim config"
echo "-------------------------------------"
echo ""
_installSymLink neovim ~/.config/neovim ~/dotfiles/neovim/ ~/.config

echo "-------------------------------------"
echo "-> Install Kitty config"
echo "-------------------------------------"
echo ""
_installSymLink kitty ~/.config/neovim ~/dotfiles/neovim/ ~/.config

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
echo "-> Creating iptables "
echo "-------------------------------------"
echo ""
sudo rm /etc/iptables/iptables.rules
_installSymLink iptables /etc/iptables/iptables.rules ~/dotfiles/iptables/iptables.rules /etc/iptables/iptables.rules
sudo systemctl restart iptables

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
echo "DONE! Please reboot your system and run 4-config.sh!"
