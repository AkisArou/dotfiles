#!/usr/bin/zsh

source ~/.env

rofi -config ~/dotfiles/rofi/"${THEME:-vscode}".rasi "$@"