#!/bin/zsh

VSCODE_PATH="/opt/visual-studio-code"
export ELECTRON_RUN_AS_NODE=1
NODE_OPTIONS=''
exec "$VSCODE_PATH"/code "$@"
