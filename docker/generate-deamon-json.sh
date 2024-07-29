#!/bin/bash

# Get the current user's home directory
HOME_DIR=$(eval echo ~$USER)

# Ensure the /etc/docker directory exists
sudo mkdir -p /etc/docker

# Write the configuration to daemon.json
echo "{
  \"data-root\": \"$HOME_DIR/docker\",
  \"bip\": \"192.168.1.1/24\"
}" | sudo tee /etc/docker/daemon.json >/dev/null
