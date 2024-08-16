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

if [ -d ~/.config ]; then
  echo ".config folder already exists."
else
  mkdir ~/.config
  echo ".config folder created."
fi

# ------------------------------------------------------
# Create symbolic links
# ------------------------------------------------------
create_symlink "$HOME/dotfiles/alacritty" "$HOME/.config/alacritty"
create_symlink "$HOME/dotfiles/kitty" "$HOME/.config/kitty"
create_symlink "$HOME/dotfiles/nvim" "$HOME/.config/nvim"
create_symlink "$HOME/dotfiles/vim/.vimrc" "$HOME/.vimrc"
create_symlink "$HOME/dotfiles/rofi" "$HOME/.config/rofi"
create_symlink "$HOME/dotfiles/dunst" "$HOME/.config/dunst"
create_symlink "$HOME/dotfiles/ssh/config" "$HOME/.ssh/config"
create_symlink "$HOME/dotfiles/xorg/.xinitrc" "$HOME/.xinitrc"
create_symlink "$HOME/dotfiles/xorg/.Xresources" "$HOME/.Xresources"
create_symlink "$HOME/dotfiles/zsh/.zprofile" "$HOME/.zprofile"
create_symlink "$HOME/dotfiles/i3" "$HOME/.config/i3"
create_symlink "$HOME/dotfiles/git" "$HOME/.config/git"
create_symlink "$HOME/dotfiles/zsh/.zshrc" "$HOME/.zshrc"
create_symlink "$HOME/dotfiles/.ideavimrc" "$HOME/.ideavimrc"
create_symlink "$HOME/dotfiles/.tmux.conf" "$HOME/.tmux.conf"
create_symlink "$HOME/dotfiles/.tool-versions" "$HOME/.tool-versions"
create_symlink "$HOME/dotfiles/pipewire" "$HOME/.config/pipewire"
create_symlink "$HOME/dotfiles/polybar" "$HOME/.config/polybar"
create_symlink "$HOME/dotfiles/doom" "$HOME/.config/doom"

# Firefox
PROFILE_DIR=$(find ~/.mozilla/firefox -type d -name '*.default-release')

if [ -z "$PROFILE_DIR" ]; then
  echo "Firefox profile directory not found."
  exit 1
fi
#
create_symlink "$HOME/dotfiles/firefox/user.js" "$PROFILE_DIR"/user.js
sudo -E ln -s "$HOME/dotfiles/firefox/policies.json" /usr/lib/firefox/distribution/policies.json

# LibreWolf
# Policies for extensions should be handled manually for now
create_symlink "$HOME/dotfiles/librewolf/librewolf.overrides.cfg" "$HOME/.librewolf/librewolf.overrides.cfg"

./docker/generate-deamon-json.sh

echo "DONE! Please reboot your system and run 4-config.sh!"
