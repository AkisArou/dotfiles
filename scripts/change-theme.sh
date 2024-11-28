#!/usr/bin/zsh

SELECTED_THEME=$1
VSCODE_THEME="vscode"
TOKYONIGHT_THEME="tokyonight"

if [ -z "$SELECTED_THEME" ] || ([ "$SELECTED_THEME" != "$VSCODE_THEME" ] && [ "$SELECTED_THEME" != "$TOKYONIGHT_THEME" ]); then
  echo "Invalid theme! \nOptions: $VSCODE_THEME, $TOKYONIGHT_THEME"
  exit 1
fi

# Rofi
sed -i "s|@import \"./.*\.rasi\"|@import \"./$SELECTED_THEME.rasi\"|" ~/dotfiles/rofi/config.rasi

# Foot
sed -i "s|include=~/dotfiles/foot/.*\.ini|include=~/dotfiles/foot/$SELECTED_THEME.ini|" ~/dotfiles/foot/foot.ini

# Waybar
sed -i "s|@import \"./.*\.css\"|@import \"./$SELECTED_THEME.css\"|" ~/dotfiles/waybar/style.css
killall waybar
waybar &
