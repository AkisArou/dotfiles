#!/usr/bin/sh

source ~/.env

foot --override include=~/dotfiles/foot/"${THEME:-vscode}".ini
