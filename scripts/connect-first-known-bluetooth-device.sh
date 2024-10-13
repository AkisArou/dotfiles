#!/bin/zsh

# Get list of devices (MAC address and device name)
devices=$(bluetoothctl devices)
# Extract both MAC address and the device name
device_info=$(echo "$devices" | grep -Eo '([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2} .+')
mac_address=$(echo "$device_info" | awk '{print $1}')
device_name=$(echo "$device_info" | awk '{for(i=2;i<=NF;++i) printf $i " "; print ""}')

echo "MAC Address: $mac_address"
echo "Device Name: $device_name"

# Check if there are connected devices
connected_status=$(bluetoothctl info "$mac_address" | grep "Connected" | awk '{print $2}')

echo "Connected status: $connected_status"

if [ "$connected_status" = "yes" ]; then
  bluetoothctl disconnect "$mac_address"
  text="Disconnected $device_name ($mac_address)"
else
  bluetoothctl connect "$mac_address"
  text="Connected $device_name ($mac_address)"
fi

echo "$text"

if [ -n "$mac_address" ]; then
  notify-send -t 1500 "$text"
fi
