#!/bin/bash

ssh_key_path="$HOME/.ssh/id_rsa"

# Check if SSH agent is running
if [ -z "$SSH_AUTH_SOCK" ] || [ -z "$SSH_AGENT_PID" ]; then
	echo "Starting SSH agent..."
	eval "$(ssh-agent -s)"
fi

# Check if SSH key exists
if [ -f "$ssh_key_path" ]; then
	echo "SSH key found. Adding to SSH agent."
	ssh-add "$ssh_key_path"
else
	echo "No SSH key found at $ssh_key_path."
fi
