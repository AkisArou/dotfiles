#!/bin/zsh

echo ""
echo "-------------------------------------"
echo "-> Enabling systemctl daemons"
echo "-------------------------------------"
echo ""
sudo systemctl enable --now bluetooth
sudo systemctl enable --now sshd
sudo systemctl enable --now avahi-daemon
sudo systemctl enable --now firewalld

echo ""
echo "-------------------------------------"
echo "-> Setting up gnome settings"
echo "-------------------------------------"
echo ""
if command -v gsettings &>/dev/null; then
  gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 33
  gsettings set org.gnome.desktop.peripherals.keyboard delay 220
  gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'gr')]"
  # Do not group applications
  gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
  dconf write /org/gnome/desktop/interface/color-scheme \'prefer-dark\'
fi

echo ""
echo "-------------------------------------"
echo "-> Setting up tmux"
echo "-------------------------------------"
echo ""
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
rm -rf ~/.tmux/plugins/tpm/.git

echo ""
echo "-------------------------------------"
echo "-> Setting up asdf"
echo "-------------------------------------"
echo ""
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.13.1

source ~/.zshrc

## Add asdf plugins
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git

# Read .tool-versions file and install the specified versions
cd ~
asdf install

echo ""
echo "-------------------------------------"
echo "-> Modified system for vite"
echo "-------------------------------------"
echo ""
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

echo ""
echo "-------------------------------------"
echo "-> Optimizing ssd"
echo "-------------------------------------"
echo ""
sudo systemctl enable fstrim.timer

echo ""
echo "-------------------------------------"
echo "-> Setting up docker"
echo "-------------------------------------"
echo ""
ls -al /dev/kvm
sudo usermod -aG kvm $USER
sudo usermod -aG docker $USER
docker --version
sudo systemctl enable docker.service
docker context use default
docker compose version

echo ""
echo "-------------------------------------"
echo "-> Configuring NodeJS"
echo "-------------------------------------"
echo ""
corepack enable pnpm
corepack enable yarn
asdf reshim nodejs
corepack use pnpm@latest

echo "Installing global npm packages..."
npm i -g npm-workspaces-language-server
