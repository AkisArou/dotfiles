#!/bin/bash

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
_installSymLink kitty ~/.config/kitty ~/dotfiles/kitty/ ~/.config
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
_installSymLink .xinitrc ~/.xinitrc ~/dotfiles/xorg/.xinitrc ~/.xinitrc
_installSymLink .Xresouces ~/.Xresources ~/dotfiles/xorg/.Xresources ~/.Xresources
_installSymLink .zprofile ~/.zprofile ~/dotfiles/zsh/.config/zsh/.zprofile ~/.zprofile

echo ""
echo "-------------------------------------"
echo "-> Install GTK themes"
echo "-------------------------------------"
echo ""
~/dotfiles/gtk/install-dracula.sh

echo "-------------------------------------"
echo "-> Install i3 dotfiles"
echo "-------------------------------------"
echo ""
_installSymLink i3 ~/.config/i3 ~/dotfiles/i3/ ~/.config/i3
_installSymLink i3status-rust ~/.config/i3status-rust ~/dotfiles/i3status-rust ~/.config/i3status-rust

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
echo "-> Install zsh config"
echo "-------------------------------------"
echo ""
_installSymLink .zshrc ~/.zshrc ~/dotfiles/zsh/.zshrc ~/.zshrc
_installSymLink .zprofile ~/.zprofile ~/dotfiles/zsh/.zprofile ~/.zprofile

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

# echo "-------------------------------------"
# echo "->  Brave/Chromium configs "
# echo "-------------------------------------"
# echo ""
# _installSymLink brave-flags ~/.config/brave-flags.conf ~/dotfiles/brave-flags.conf ~/.config
# _installSymLink chromium-flags ~/.config/chromium-flags.conf ~/dotfiles/chromium-flags.conf ~/.config

echo "-------------------------------------"
echo "->  Pipewire"
echo "-------------------------------------"
echo ""
_installSymLink pipewire ~/.config/pipewire ~/dotfiles/pipewire ~/.config

echo "-------------------------------------"
echo "->  Polybar"
echo "-------------------------------------"
echo ""
_installSymLink polybar ~/.config/polybar ~/dotfiles/polybar ~/.config

echo "-------------------------------------"
echo "->  Docker"
echo "-------------------------------------"
echo ""
./docker/generate-deamon-json.sh

echo "-------------------------------------"
echo "->  Doom Emacs"
echo "-------------------------------------"
echo ""
_installSymLink doom ~/.config/doom ~/dotfiles/doom ~/.config

echo "-------------------------------------"
echo "->  Firefox"
echo "-------------------------------------"
echo ""
PROFILE_DIR=$(find ~/.mozilla/firefox -type d -name '*.default-release')

if [ -z "$PROFILE_DIR" ]; then
  echo "Firefox profile directory not found."
  exit 1
fi

_installSymLink firefox "$PROFILE_DIR"/user.js ~/dotfiles/firefox/user.js "$PROFILE_DIR"
sudo ln -s ~/dotfiles/firefox/policies.json /usr/lib/firefox/distribution/policies.json

echo "-------------------------------------"
echo "->  yazi"
echo "-------------------------------------"
echo ""
_installSymLink yazi ~/.config/yazi ~/dotfiles/yazi ~/.config

# ------------------------------------------------------
# DONE
# ------------------------------------------------------
echo "DONE! Please reboot your system and run 4-config.sh!"
