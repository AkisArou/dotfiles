#!/bin/bash

# Path to the chrome.desktop file
desktop_file="/usr/share/applications/brave-browser.desktop"

# Command to be added to the Exec line
new_command="brave --incognito --remote-debugging-port=9229"

# Check if the file exists
if [ -e "$desktop_file" ]; then
    # Modify the Exec line
    sed -i "s/^Exec=.*/Exec=$new_command/" "$desktop_file"

    echo "Brave desktop file updated successfully."
else
    echo "Error: Brave desktop file not found at $desktop_file."
fi
