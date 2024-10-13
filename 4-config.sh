#!/bin/zsh

echo "Enabling systemctl daemons..."
sudo systemctl enable --now iwd
sudo systemctl enable --now bluetooth
sudo systemctl enable --now sshd
sudo systemctl enable --now avahi-daemon

echo "Setting up gnome settings..."
if command -v gsettings &>/dev/null; then
  gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 33
  gsettings set org.gnome.desktop.peripherals.keyboard delay 220
  gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'gr')]"
  # Do not group applications
  gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
  dconf write /org/gnome/desktop/interface/color-scheme \'prefer-dark\'
fi

echo "Setting up tmux..."
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
~/.tmux/plugins/tpm/scripts/install_plugins.sh
tmux source ~/.tmux.conf

echo "Setting up asdf..."
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.13.1

source ~/.zshrc

## Add asdf plugins
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin add java https://github.com/halcyon/asdf-java.git
asdf plugin add kotlin https://github.com/asdf-community/asdf-kotlin.git
asdf plugin add maven

# Read .tool-versions file and install the specified versions
cd ~
asdf install

echo "Configuring java..."
. ~/.asdf/plugins/java/set-java-home.zsh

echo "Modifying system for vite..."
# Check current limit
ulimit -Sn
# Change limit (temporary)
ulimit -Sn 10000 # You might need to change the hard limit too

# Check current limits
sysctl fs.inotify
# Change limits (temporary)
sudo sysctl fs.inotify.max_queued_events=16384
sudo sysctl fs.inotify.max_user_instances=8192
sudo sysctl fs.inotify.max_user_watches=524288

echo "* - nofile 65536" | sudo tee -a /etc/security/limits.conf
echo "DefaultLimitNOFILE=65536" | sudo tee -a /etc/systemd/system.conf
echo "DefaultLimitNOFILE=65536" | sudo tee -a /etc/systemd/user.conf

echo "Optimizing ssd..."
sudo systemctl enable fstrim.timer

echo "Setting up docker..."
ls -al /dev/kvm
sudo usermod -aG kvm $USER
sudo usermod -aG docker $USER
docker --version
# .socket instead of .service to start docker deamon only when needed
sudo systemctl enable --now docker.socket
docker context use default
docker compose version

echo "Configuring NodeJS..."
corepack enable pnpm
corepack enable yarn
asdf reshim nodejs

echo "Installing global npm packages..."
npm i -g npm-workspaces-language-server

echo "Setting xdg default web browser..."
ORIGINAL_BROWSER=$BROWSER
unset BROWSER
xdg-settings set default-web-browser librewolf.desktop
export BROWSER=$ORIGINAL_BROWSER
