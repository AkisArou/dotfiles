#!/usr/bin/zsh

if [ -f "$HOME/dotfiles/eza/${THEME}.yml" ]; then
    ln -sf "$HOME/dotfiles/eza/${THEME}.yml" "$HOME/.config/eza/theme.yml"
    echo "Symlink created: $HOME/.config/eza/theme.yml -> $HOME/dotfiles/eza/${THEME}.yml"
else
    echo "eza theme file does not exist. Applying default"
    ln -sf "$HOME/dotfiles/eza/onedark.yml" "$HOME/.config/eza/theme.yml"
fi
