#!/usr/bin/zsh

# Display a Rofi menu with the title "Theme" and two options: "vscode" and "tokyonight"
theme=$(echo -e "vscode\ntokyonight" | ~/dotfiles/rofi/launch-rofi.sh -dmenu -p "Theme")

~/dotfiles/scripts/change-theme.sh $theme
