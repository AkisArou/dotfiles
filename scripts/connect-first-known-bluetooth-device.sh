#!/bin/bash

dev=$(bluetoothctl devices)
mac_address=$(echo "$dev" | grep -oE '([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}')
echo $mac_address

# Check if there are connected devices
connected_devices=$(bluetoothctl info | grep "Connected" | awk '{print $2}')

if [ "$connected_devices" == "yes" ]; then
	bluetoothctl disconnect $mac_address
	echo "Disconnected from $mac_address"
else
	bluetoothctl connect $mac_address
	echo "Connected to $mac_address"
fi
