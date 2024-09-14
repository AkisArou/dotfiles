kill_by_port() {
  if [[ -n "$1" ]]; then
    local port="$1"
    local process_id
    process_id=$(lsof -ti :"$port")

    if [[ -n "$process_id" ]]; then
      kill -9 "$process_id"
      echo "Killed process with PID $process_id using port $port."
    else
      echo "No process found using port $port."
    fi
  else
    echo "Usage: kill_process_by_port <port>"
  fi
}

wake_desktop() {
  wol "$DESKTOP_MAC"
}

sus_desktop() {
  ssh "$DESKTOP" 'sudo -S systemctl suspend'
}

ccd() {
  local dir
  dir="$(
    find "${1:-.}" -path '*/\.*' -prune -o -type d -print 2>/dev/null |
      fzf +m \
        --preview='tree -C {} | head -n $FZF_PREVIEW_LINES' \
        --preview-window='right:hidden:wrap' \
        --bind=ctrl-v:toggle-preview \
        --bind=ctrl-x:toggle-sort \
        --header='(view:ctrl-v) (sort:ctrl-x)'
  )" || return
  cd "$dir" || return
}

fv() {
  local file
  file="$(
    find "${1:-.}" -path '*/\.*' -prune -o -type f -print 2>/dev/null |
      fzf +m \
        --preview='tree -C {} | head -n $FZF_PREVIEW_LINES' \
        --preview-window='right:hidden:wrap' \
        --bind=ctrl-v:toggle-preview \
        --bind=ctrl-x:toggle-sort \
        --header='(view:ctrl-v) (sort:ctrl-x)'
  )" || return
  nvim "$file" || return
}
