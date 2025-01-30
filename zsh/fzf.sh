export FZF_DEFAULT_COMMAND="fd --type file --follow --hidden --exclude .git"

FZF_TOKYONIGHT="\
  --color=bg+:#2e3c64 \
  --color=bg:#1f2335 \
  --color=border:#29a4bd \
  --color=fg:#c0caf5 \
  --color=gutter:#1f2335 \
  --color=header:#ff9e64 \
  --color=hl+:#2ac3de \
  --color=hl:#2ac3de \
  --color=info:#545c7e \
  --color=marker:#ff007c \
  --color=pointer:#ff007c \
  --color=prompt:#2ac3de \
  --color=query:#c0caf5:regular \
  --color=scrollbar:#29a4bd \
  --color=separator:#ff9e64 \
  --color=spinner:#ff007c \
"

FZF_VSCODE="\
  --color=bg:#1f1f1f \
  --color=bg+:#222222 \
  --color=border:#1f1f1f \
  --color=fg:#d4d4d4 \
  --color=gutter:#2D2D2D \
  --color=header:#646695 \
  --color=hl:#C586C0 \
  --color=hl+:#608b4e \
  --color=info:#8db9e2 \
  --color=marker:#4EC9B0 \
  --color=pointer:#4EC9B0 \
  --color=prompt:#C586C0 \
  --color=query:#d4d4d4:regular \
  --color=scrollbar:#2d2d2d \
  --color=separator:#223e55 \
  --color=spinner:#4ec9b0 \
"

FZF_GRUVBOX="\
  --color=bg:#282828 \
  --color=bg+:#3c3836 \
  --color=border:#282828 \
  --color=fg:#d4be98 \
  --color=gutter:#3c3836 \
  --color=header:#7daea3 \
  --color=hl:#d3869b \
  --color=hl+:#89b482 \
  --color=info:#7daea3 \
  --color=marker:#89b482 \
  --color=pointer:#89b482 \
  --color=prompt:#d3869b \
  --color=query:#d4be98:regular \
  --color=scrollbar:#3c3836 \
  --color=separator:#7daea3 \
  --color=spinner:#89b482 \
"

FZF_ONEDARK="\
  --color=bg:#1a1d21 \
  --color=bg+:#222222 \
  --color=border:#1f1f1f \
  --color=fg:#dcdfe4 \
  --color=gutter:#2D2D2D \
  --color=header:#919baa \
  --color=hl:#c678dd \
  --color=hl+:#98c379 \
  --color=info:#8db9e2 \
  --color=marker:#56b6c2 \
  --color=pointer:#56b6c2 \
  --color=prompt:#c678dd \
  --color=query:#d4d4d4:regular \
  --color=scrollbar:#313640 \
  --color=separator:#223e55 \
  --color=spinner:#56b6c2 \
"

FZF_THEME="FZF_$(echo "$THEME" | tr '[:lower:]' '[:upper:]')"

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS \
  --bind ctrl-n:down,ctrl-p:up,ctrl-d:page-down,ctrl-u:page-up,ctrl-e:accept,ctrl-k:next-history,ctrl-j:prev-history \
  --highlight-line \
  --info=inline-right \
  --ansi \
  --layout=reverse \
  --border=none \
  --history=$HOME/.fzf_history
  $(eval echo \$"$FZF_THEME")
"
