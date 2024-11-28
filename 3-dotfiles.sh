#!/bin/zsh

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Applying application configs..."
echo "-------------------------------"
echo ""

if [ -d ~/.config ]; then
  print_info ".config folder already exists."
else
  mkdir ~/.config
  print_success ".config folder created."
fi

# Create symbolic links

# create dirs
mkdir -p "$HOME/.ssh"
mkdir -p "$HOME/.local/share/applications"
mkdir -p "$HOME/.config/BraveSoftware/Brave-Browser/Default/Web Applications/Manifest Resources/"
sudo mkdir -p "/usr/share/chromium/extensions"

# linking
sudo ln -sf "$HOME/dotfiles/environment" /etc/environment

create_symlink "$HOME/dotfiles/alacritty" "$HOME/.config/alacritty"
create_symlink "$HOME/dotfiles/kitty" "$HOME/.config/kitty"
create_symlink "$HOME/dotfiles/nvim" "$HOME/.config/nvim"
create_symlink "$HOME/dotfiles/vim/.vimrc" "$HOME/.vimrc"
create_symlink "$HOME/dotfiles/rofi" "$HOME/.config/rofi"
create_symlink "$HOME/dotfiles/dunst" "$HOME/.config/dunst"
create_symlink "$HOME/dotfiles/ssh/config" "$HOME/.ssh/config"
create_symlink "$HOME/dotfiles/xorg/.xinitrc" "$HOME/.xinitrc"
create_symlink "$HOME/dotfiles/xorg/.xscreensaver" "$HOME/.xscreensaver"
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
create_symlink "$HOME/dotfiles/zed" "$HOME/.config/zed"
create_symlink "$HOME/dotfiles/doom" "$HOME/.config/doom"
create_symlink "$HOME/dotfiles/btop" "$HOME/.config/btop"
create_symlink "$HOME/dotfiles/yazi" "$HOME/.config/yazi"
create_symlink "$HOME/dotfiles/sway" "$HOME/.config/sway"
create_symlink "$HOME/dotfiles/waybar" "$HOME/.config/waybar"
create_symlink "$HOME/dotfiles/foot" "$HOME/.config/foot"
create_symlink "$HOME/dotfiles/eza" "$HOME/.config/eza"
create_symlink "$HOME/dotfiles/starship" "$HOME/.config/starship"
create_symlink "$HOME/dotfiles/code/code-flags.conf" "$HOME/.config/code-flags.conf"
create_symlink "$HOME/dotfiles/fontconfig" "$HOME/.config/fontconfig"
create_symlink "$HOME/dotfiles/brave/brave-flags.conf" "$HOME/.config/brave-flags.conf"
sudo ln -sf "$HOME/dotfiles/brave/extensions/fmkadmapgofadopljbjfkapdkoienihi.json" "/usr/share/chromium/extensions/fmkadmapgofadopljbjfkapdkoienihi.json"
sudo ln -sf "$HOME/dotfiles/brave/extensions/nngceckbapebfimnlniiiahkandclblb.json" "/usr/share/chromium/extensions/nngceckbapebfimnlniiiahkandclblb.json"

sudo chmod 644 /usr/share/chromium/extensions/fmkadmapgofadopljbjfkapdkoienihi.json
sudo chmod 644 /usr/share/chromium/extensions/nngceckbapebfimnlniiiahkandclblb.json

# Librewolf
LIBREWOLF_CONFIG="$HOME/.librewolf/librewolf.overrides.cfg"

if [ ! -f "$LIBREWOLF_CONFIG" ]; then
  mkdir -p "$(dirname "$LIBREWOLF_CONFIG")"
  touch "$LIBREWOLF_CONFIG"
fi

create_symlink "$HOME/dotfiles/librewolf/librewolf.overrides.cfg" "$LIBREWOLF_CONFIG"

sudo -E python3 ~/dotfiles/librewolf/merge-policies.py

# Docker
./docker/generate-deamon-json.sh

print_success "DONE!"
