#!/bin/zsh

if ! updates=$(paru -Qu 2>/dev/null | wc -l); then
  updates=0
fi

if [ "$updates" -gt 0 ]; then
  echo " $updates"
else
  echo "0"
fi
