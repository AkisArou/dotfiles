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

paru() {
  if [ "$#" -eq 0 ]; then
    # No arguments provided, just run paru
    command paru && ~/dotfiles/polybar/scripts/refresh-updates.sh
    return $?
  else
    local command="$1"

    if [[ "$command" == "-Syu" ]]; then
      command paru -Syu && ~/dotfiles/polybar/scripts/refresh-updates.sh
      return $?
    else
      shift
      local args="$@"

      # Run paru with the provided command and arguments
      if command paru "$command" "$args"; then
        # Run refresh-updates.sh only if the command is related to installing, removing, or updating
        if [[ "$command" == "-S" || "$command" == "-R" ]]; then
          ~/dotfiles/polybar/scripts/refresh-updates.sh
        fi
      else
        # Return the exit status of paru if it fails
        return $?
      fi
    fi

  fi
}
