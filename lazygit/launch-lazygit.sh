#!/usr/bin/zsh

source ~/.env

/usr/bin/lazygit --use-config-file=$HOME/dotfiles/lazygit/config.yml,$HOME/dotfiles/lazygit/"${THEME:-onedark}".yml "$@"
