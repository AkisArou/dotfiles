#!/bin/bash

# Initialize variables with default values
silent_mode=false

# Parse command-line options
while getopts ":s" opt; do
	case ${opt} in
	s)
		silent_mode=true
		;;
	\?)
		echo "Usage: $0 [-s|--silent]" >&2
		exit 1
		;;
	esac
done

silent_mode=false

# Check if SSH agent is running
if [ -z "$SSH_AUTH_SOCK" ] || [ -z "$SSH_AGENT_PID" ]; then
	! $silent_mode && echo "Starting SSH agent..."
	eval "$(ssh-agent -s)" >/dev/null 2>&1
fi

ssh_key_path="$HOME/.ssh/id_rsa"
added_keys=$(ssh-add -l)

# Check if already added
if [ -n "$added_keys" ]; then
	! $silent_mode && echo "SSH keys are added."
	exit 0
else
	! $silent_mode && echo "No SSH keys added."
fi

# Check if SSH key exists
if [ -f "$ssh_key_path" ]; then
	! $silent_mode && echo "SSH key found. Adding to SSH agent."
	ssh-add "$ssh_key_path" >/dev/null 2>&1
else
	! $silent_mode && echo "No SSH key found at $ssh_key_path."
fi
