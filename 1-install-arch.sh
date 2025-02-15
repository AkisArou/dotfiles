#!/bin/bash

source "$(dirname "$0")/scripts/library.sh"

clear
echo "Installing base packages and applying base config..."
echo "----------------------------------------------------"
echo ""

# Paru
if sudo pacman -Qs paru >/dev/null; then
  print_success "paru is installed..."
else
  print_info "paru is not installed. Will be installed now!"
  git clone https://aur.archlinux.org/paru.git ~/paru
  cd ~/paru || exit
  makepkg -si
  cd ~/dotfiles/ || return
  rm -rf ~/paru
  print_success "paru has been installed successfully."
fi

# Install Packages
packages=(
  "mesa"
  "vulkan-tools"
  "audit"
  "git"
  "fwupd"
  "udisks2" # Needed for fwupd
  "linux-headers"
  "update-grub"
  "base"
  "base-devel"
  "posix"
  "acpi"
  "wol"
  "libinput"
  "libva-utils"
  "libvdpau-va-gl"
  "vdpauinfo"
  "ffmpeg"
  "rsync"
  "preload"
  "power-profiles-daemon"
  "wl-clipboard"
  "wget"
  "curl"
  "htop"
  "btop"
  "tldr"
  "bleachbit"
  "yazi"
  "pipewire"
  "pipewire-alsa"
  "pipewire-audio"
  "pipewire-pulse"
  "pipewire-jack"
  "wireplumber"
  "xdg-user-dirs"
  "xdg-utils"
  "avahi"
  "wpa_supplicant"
  "iwd"
  "openssh"
  "mpv"
  "zsh"
  "alacritty"
  "kitty"
  "brave-bin"
  "librewolf-bin"
  "dunst"
  "thunar"
  "adw-gtk-theme"
  "ttf-hack-nerd"
  "ttf-font-awesome"
  "ttf-meslo-nerd"
  "ttf-dm-mono-git"
  "noto-fonts-emoji"
  "terminus-font"
  "eza"
  "bat"
  "python-pip"
  "xdg-desktop-portal-gtk"
  "pavucontrol"
  "bluez"
  "bluez-utils"
  "brightnessctl"
  "7zip"
  "fastfetch"
  "ripgrep"
  "fd"
  "gvim"
  "visual-studio-code-bin"
  "tmux"
  "git-delta"
  "gnome-terminal"
  "docker"
  "docker-compose"
  "fzf"
  "jq"
  "ueberzugpp"
  "imagemagick"
  "ghostscript"
  "pnpm-shell-completion"
  "libreoffice-still"
  "figlet"
  "starship"
  "android-studio"
  "genymotion"
  "asdf-vm"
  "unzip" # Needed by asdf
  "rustup"
  "ccache" # For faster nvim builds
  "lazygit"
  "github-cli"
)

~/dotfiles/scripts/create-template-env.sh

# Install required packages
install_packages "${packages[@]}"

~/dotfiles/scripts/build-nvim.sh

zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1

# ST
print_info "Making st..."
sudo make -C ~/dotfiles/st clean install

# Install custom issue (login prompt)
echo ""
print_info "Installing login screen..."
sudo cp ~/dotfiles/login/issue /etc/issue

# Check if the current shell is zsh
if [[ "$SHELL" != */zsh ]]; then
  print_info "Current shell is not zsh. Changing default shell to zsh..."
  chsh -s $(which zsh)
else
  print_success "Current shell is already zsh."
fi

print_success "DONE!"
