#!/bin/zsh

### THEME

THEME_URL="https://github.com/dracula/gtk/archive/master.zip"
CONFIG_DIR="$HOME/.config"
TEMP_ZIP="/tmp/Dracula.zip"

mkdir -p $CONFIG_DIR

echo "Downloading Dracula GTK theme..."
curl -L $THEME_URL -o $TEMP_ZIP

echo "Extracting theme..."
unzip -qo $TEMP_ZIP -d /tmp

echo "Moving files to .config directory..."
mv /tmp/gtk-master/* $CONFIG_DIR

rm -rf $TEMP_ZIP /tmp/gtk-master

echo "Setting GTK theme and window manager preferences..."
gsettings set org.gnome.desktop.interface gtk-theme "Dracula"
gsettings set org.gnome.desktop.wm.preferences theme "Dracula"

echo "Theme installed and activated successfully!"
