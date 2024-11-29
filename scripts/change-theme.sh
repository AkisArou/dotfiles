#!/usr/bin/zsh

SELECTED_THEME=$1
VSCODE_THEME="vscode"
TOKYONIGHT_THEME="tokyonight"

if [ -z "$SELECTED_THEME" ] || ([ "$SELECTED_THEME" != "$VSCODE_THEME" ] && [ "$SELECTED_THEME" != "$TOKYONIGHT_THEME" ]); then
  echo "Invalid theme! \nOptions: $VSCODE_THEME, $TOKYONIGHT_THEME"
  exit 1
fi

# rofi
sed -i "s|@import \"./.*\.rasi\"|@import \"./$SELECTED_THEME.rasi\"|" ~/dotfiles/rofi/config.rasi

# foot
sed -i "s|include=~/dotfiles/foot/.*\.ini|include=~/dotfiles/foot/$SELECTED_THEME.ini|" ~/dotfiles/foot/foot.ini

# waybar
sed -i "s|@import \"./.*\.css\"|@import \"./$SELECTED_THEME.css\"|" ~/dotfiles/waybar/style.css
killall waybar
waybar &

# yazi
sed -i "s/use = \"[^\"]*\"/use = \"$SELECTED_THEME\"/" ~/dotfiles/yazi/theme.toml

# eza
sed -i "s|export EZA_CONFIG_DIR=\"[^\"]*\"|export EZA_CONFIG_DIR=\"~/dotfiles/eza/$SELECTED_THEME\"|" ~/dotfiles/zsh/exports.sh

# nvim
sed -i "s|local selectedTheme = themes\.[a-zA-Z0-9_]*|local selectedTheme = themes.$SELECTED_THEME|" ~/dotfiles/nvim/lua/plugins/colorscheme.lua

for addr in $XDG_RUNTIME_DIR/nvim.*; do
  nvim --server $addr --remote-send ":colorscheme $SELECTED_THEME<CR>"
done

# fzf
CAPITALIZED_THEME=$(echo "$SELECTED_THEME" | awk '{print toupper($0)}')
sed -i "s/FZF_SELECTED_THEME=\$FZF_[A-Z]*/FZF_SELECTED_THEME=\$FZF_${CAPITALIZED_THEME}/" ~/dotfiles/zsh/exports.sh

# tmux
sed -i "s|source-file ~/dotfiles/tmux/[a-zA-Z]*.tmux|source-file ~/dotfiles/tmux/${SELECTED_THEME}.tmux|" ~/dotfiles/.tmux.conf
tmux source ~/.tmux.conf

#btop
sed -i "s|color_theme = \".*\"|color_theme = \"${SELECTED_THEME}\"|" ~/dotfiles/btop/btop.conf

source ~/dotfiles/zsh/exports.sh
