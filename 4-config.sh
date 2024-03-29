!/bin/bash

reload_shell_configuration() {
	echo "Reloading shell configuration"
	. "$${HOME}/.zshrc" 2>/dev/null
	echo "Shell configuration reloaded"
}

echo ""
echo "-------------------------------------"
echo "-> Setting up gnome settings"
echo "-------------------------------------"
echo ""
if command -v gsettings &>/dev/null; then
	gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30
	gsettings set org.gnome.desktop.peripherals.keyboard delay 250
	gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'gr')]"
	# Do not group applications
	gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
	dconf write /org/gnome/desktop/interface/color-scheme \'prefer-dark\'
fi

echo ""
echo "-------------------------------------"
echo "-> Setting up VirtualBox"
echo "-------------------------------------"
echo ""
sudo usermod -a -G vboxusers $USER
modprobe vboxdrv

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

## Add asdf plugins
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin add java https://github.com/halcyon/asdf-java.git
asdf plugin-add maven
asdf plugin-add helm https://github.com/Antiarchitect/asdf-helm.git
asdf plugin add kotlin https://github.com/asdf-community/asdf-kotlin.git

reload_shell_configuration

# Read .tool-versions file and install the specified versions
asdf install

# Set installed versions as global
while read -r line; do
	plugin=$(echo "$line" | cut -d " " -f 1)
	version=$(echo "$line" | cut -d " " -f 2)
	asdf global "$plugin" "$version"
done <~/.tool-versions

### NodeJS
NODE_VERSION=$(cat ~/.tool-versions | grep nodejs | cut -d " " -f 2)
asdf global nodejs "$NODE_VERSION"

### Java
JAVA_VERSION=$(cat ~/.tool-versions | grep java | cut -d " " -f 2)

asdf global java "$JAVA_VERSION"
echo "Set global java version"
. ~/.asdf/plugins/java/set-java-home.zsh
. ~/.asdf/plugins/java/set-java-home.bash
. ~/.asdf/plugins/java/set-java-home.fish

### Maven
MAVEN_VERSION=$(cat ~/.tool-versions | grep maven | cut -d " " -f 2)
asdf global maven "$MAVEN_VERSION"
echo "Set global maven version"

### Kotlin
KOTLIN_VERSION=$(cat ~/.tool-versions | grep kotlin | cut -d " " -f 2)
asdf global kotlin "$KOTLIN_VERSION"
echo "Set global kotlin version"

echo ""
echo "-------------------------------------"
echo "-> Modified system for vite web build tool"
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
systemctl --user enable docker-desktop
sudo systemctl enable docker.service
sudo systemctl enable containerd.service
docker context use default
docker compose version
echo "You also have to login into docker-desktop"
echo "https://docs.docker.com/desktop/get-started/#credentials-management-for-linux-users"

reload_shell_configuration

echo ""
echo "-------------------------------------"
echo "-> Installing yarn globally"
echo "-------------------------------------"
echo ""
npm i -g yarn

echo ""
echo "-------------------------------------"
echo "-> Installing global npm packages"
echo "-------------------------------------"
echo ""
npm i -g npm-workspaces-lsp
npm i -g css-variables-language-server

echo ""
echo "-------------------------------------"
echo "-> Changing shell to zsh"
echo "-------------------------------------"
echo ""
sudo chsh -s /usr/bin/zsh
echo "Reboot to get zsh shell"
