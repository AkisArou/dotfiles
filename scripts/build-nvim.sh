#!/bin/sh

# Directory where Neovim will be installed
INSTALL_DIR="$HOME/neovim"

# Repository URL
REPO_URL="https://github.com/neovim/neovim"

# Function to build Neovim
build_neovim() {
    echo "Building Neovim..."
    cd "$INSTALL_DIR" || exit
    make CMAKE_BUILD_TYPE=RelWithDebInfo CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX=$INSTALL_DIR"
    make install
}

# Check if the directory exists
if [ -d "$INSTALL_DIR" ]; then
    echo "Neovim installation found. Updating..."
    cd "$INSTALL_DIR" || exit
    git pull --rebase
    build_neovim
else
    echo "Neovim not found. Cloning and building..."
    git clone "$REPO_URL" "$INSTALL_DIR"
    cd "$INSTALL_DIR" || exit
    build_neovim
fi

# Add Neovim to PATH (temporary for current session)
export PATH="$INSTALL_DIR/bin:$PATH"
echo "Neovim is installed and up-to-date."
