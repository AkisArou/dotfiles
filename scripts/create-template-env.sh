#!/usr/bin/sh

if [ ! -f "$HOME/.env" ]; then
  touch "$HOME/.env"
  echo 'DESKTOP_MAC=""' >>"$HOME/.env"
  echo "THEME=vscode" >>"$HOME/.env"
  echo "Created ~/.env file."
fi
