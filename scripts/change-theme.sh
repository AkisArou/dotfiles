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

# Yazi
sed -i "s/use = \"[^\"]*\"/use = \"$SELECTED_THEME\"/" ~/dotfiles/yazi/theme.toml

# Eza
sed -i "s|export EZA_CONFIG_DIR=\"[^\"]*\"|export EZA_CONFIG_DIR=\"~/dotfiles/eza/$SELECTED_THEME\"|" ~/dotfiles/zsh/exports.sh

# Nvim
sed -i "s|local selectedTheme = themes\.[a-zA-Z0-9_]*|local selectedTheme = themes.$SELECTED_THEME|" ~/dotfiles/nvim/lua/plugins/colorscheme.lua

for addr in $XDG_RUNTIME_DIR/nvim.*; do
  nvim --server $addr --remote-send ":colorscheme $SELECTED_THEME<CR>"
done

# Fzf
CAPITALIZED_THEME=$(echo "$SELECTED_THEME" | awk '{print toupper($0)}')
sed -i "s/FZF_SELECTED_THEME=\$FZF_[A-Z]*/FZF_SELECTED_THEME=\$FZF_${CAPITALIZED_THEME}/" ~/dotfiles/zsh/exports.sh
