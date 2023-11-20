# ML4W dotfiles

This is the configuration of my Arch linux based installation with Qtile (Wayland & Xorg).

## Common Packages

- Terminal: alacritty
- Editor: nvim
- Prompt: starship
- Icons: Font Awesome
- Menus: Rofi
- Browsers: brave, chromium
- Filemanager: Neutilus, ranger, Thunar
- Cursor: Bibata Modern Classic
- Icons: Kora-Icon-Theme
- Theme: Breeze-dark

## Qtile
- Compositor: picom
- Screenshots: scrot


## Screenshots & Video

<a href="https://youtu.be/ELEQh0z3lm8" target="_blank"><img src="screenshots/screenshot-23-1.png" alt="Click to watch on YouTube" /></a>

<a href="https://youtu.be/ELEQh0z3lm8" target="_blank"><img src="screenshots/screenshot-23-2.png" alt="Click to watch on YouTube" /></a>

<a href="https://youtu.be/ELEQh0z3lm8" target="_blank"><img src="screenshots/screenshot-23-3.png" alt="Click to watch on YouTube" /></a>

<a href="https://youtu.be/ELEQh0z3lm8" target="_blank">Watch on YouTube</a>

<b><a href="https://gitlab.com/stephan-raabe/dotfiles/-/tree/main/screenshots?ref_type=heads">You can find more screenshots here.</a></b>

## Getting started

To make it easy for you to get started with my dotfiles, here's a list of recommended next steps.

PLEASE BACKUP YOUR EXISTING .config WITH YOUR DOTFILES BEFORE STARTING THE SCRIPTS.

```
# Make sure that you're in your home directory
cd

# Clone the repository from your home directory
git clone https://gitlab.com/stephan-raabe/dotfiles.git

# Or download the lastest version and unzip into ~/dotfiles folder

# Change into the new dotfiles folder
cd dotfiles

# Install all required packages
./1-install.sh

# OR/AND Install qtile window manager
./2-install-qtile.sh

# Install dotfiles
./3-install-dotfiles.sh

```
Please note that every Arch Linux system is different and I cannot guarantee that everything works fine on your system.

