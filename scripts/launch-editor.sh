#!/bin/zsh

filename=$1
line=$2
column=$3

nvim --server /tmp/nvim.pipe --remote-send "<ESC>:e +$line $filename<CR>:normal $column|<CR>"
