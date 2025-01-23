#!/usr/bin/zsh

theme=$(echo -e "vscode\ntokyonight\nonedark\ngruvbox" | ~/dotfiles/rofi/launch-rofi.sh -dmenu -p "Theme")

~/dotfiles/scripts/change-theme.sh $theme
