#!/bin/zsh

if [ -z "$SSH_AUTH_SOCK" ]; then
  eval "$(ssh-agent -s)" >/dev/null 2>&1
  ssh-add >/dev/null 2>&1
  ssh-add -l >/dev/null 2>&1
fi
