#!/usr/bin/zsh

SELECTED_THEME=$1
VSCODE_THEME="vscode"
TOKYONIGHT_THEME="tokyonight"

if [ -z "$SELECTED_THEME" ] || ([ "$SELECTED_THEME" != "$VSCODE_THEME" ] && [ "$SELECTED_THEME" != "$TOKYONIGHT_THEME" ]); then
  echo "Invalid theme! \nOptions: $VSCODE_THEME, $TOKYONIGHT_THEME"
  exit 1
fi

# Check if ~/.env file exists
if [ ! -f "$HOME/.env" ]; then
  # Create ~/.env file if it doesn't exist
  touch "$HOME/.env"
  echo "Created ~/.env file."
fi

# Use sed for in-place editing to replace or add THEME=$SELECTED_THEME
sed -i "s/^THEME=.*/THEME=$SELECTED_THEME/" "$HOME/.env" 2>/dev/null

# If THEME was not replaced, add it
if ! grep -q "^THEME=" "$HOME/.env"; then
  echo "THEME=$SELECTED_THEME" >>"$HOME/.env"
fi

echo "THEME set to $SELECTED_THEME in ~/.env"

# waybar
killall waybar
~/dotfiles/waybar/launch-waybar.sh &

# nvim
for addr in $XDG_RUNTIME_DIR/nvim.*; do
  nvim --server $addr --remote-send ":colorscheme $SELECTED_THEME<CR>"
done

# tmux
tmux source ~/dotfiles/tmux/.tmux.conf
tmux source ~/dotfiles/tmux/"$SELECTED_THEME".tmux

export THEME=$SELECTED_THEME
source ~/.zshrc
