#!/bin/zsh

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Applying system config..."
echo "-------------------------"

sudo usermod -aG wheel,audio,video "$USER"

print_info "Enabling systemctl daemons..."
loginctl enable-linger
sudo systemctl enable --now iwd
sudo systemctl enable --now bluetooth
sudo systemctl enable --now sshd
sudo systemctl enable --now avahi-daemon
sudo systemctl enable --now power-profiles-daemon

print_info "Setting up gnome settings..."
if command -v gsettings &>/dev/null; then
  gsettings set org.gnome.desktop.interface gtk-theme Default-dark
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
  gsettings set org.gnome.desktop.interface icon-theme Adwaita
  gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 33
  gsettings set org.gnome.desktop.peripherals.keyboard delay 220
  gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'gr')]"
  gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"
fi

print_info "Setting up tmux..."
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
~/.tmux/plugins/tpm/scripts/install_plugins.sh
tmux source ~/.tmux.conf

print_info "Setting up asdf..."
# Generate asdf completions
COMPLETIONS_DIR="${ASDF_DATA_DIR:-$HOME/.asdf}/completions"

if [ ! -d "$COMPLETIONS_DIR" ]; then
  mkdir -p "$COMPLETIONS_DIR"
  asdf completion zsh >"$COMPLETIONS_DIR/_asdf"
fi

## Add asdf plugins
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin add java https://github.com/halcyon/asdf-java.git

# Read .tool-versions file and install the specified versions
cd ~
asdf install

print_info "Configuring java... (for the current session only)"
. ~/.asdf/plugins/java/set-java-home.zsh

print_info "Modifying system for vite..."
# Check current limit
ulimit -Sn
# Change limit (temporary)
ulimit -Sn 10000 # You might need to change the hard limit too

# Change limits (temporary)
sudo sysctl fs.inotify.max_queued_events=16384
sudo sysctl fs.inotify.max_user_instances=8192
sudo sysctl fs.inotify.max_user_watches=524288

grep -qxF '* - nofile 65536' /etc/security/limits.conf || echo '* - nofile 65536' | sudo tee -a /etc/security/limits.conf
grep -qxF 'DefaultLimitNOFILE=65536' /etc/systemd/system.conf || echo 'DefaultLimitNOFILE=65536' | sudo tee -a /etc/systemd/system.conf
grep -qxF 'DefaultLimitNOFILE=65536' /etc/systemd/user.conf || echo 'DefaultLimitNOFILE=65536' | sudo tee -a /etc/systemd/user.conf

print_info "Setting up docker..."
ls -al /dev/kvm
sudo usermod -aG kvm $USER
sudo usermod -aG docker $USER
docker --version
# .socket instead of .service to start docker deamon only when needed
sudo systemctl enable --now docker.socket
docker context use default
docker compose version

print_info "Configuring NodeJS..."
corepack enable pnpm
corepack enable yarn
asdf reshim nodejs

print_info "Installing global npm packages..."
npm i -g npm-workspaces-language-server

print_info "Setting xdg default web browser..."
ORIGINAL_BROWSER=$BROWSER
unset BROWSER
xdg-settings set default-web-browser brave-browser.desktop
export BROWSER=$ORIGINAL_BROWSER

print_info "Installing yazi plugins..."
ya pack -a yazi-rs/plugins:git

print_info "Building bat cache..."
bat cache --build

print_info "Generating zsh completions..."
~/dotfiles/scripts/setup-completions.sh
source "$HOME/dotfiles/scripts/try-source-completions.sh"

print_success "DONE!"
