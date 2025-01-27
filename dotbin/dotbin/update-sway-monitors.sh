#!/bin/bash

# Define the path to the monitors config file
# monitors_config="/tmp/test"
monitors_config="/home/steven/.config/sway/monitors"

# Get the name of the left monitor
leftmon=$(swaymsg -t get_outputs -r | jq -r '.[] | select(.rect.x == 0 and .active == true) | .name')

# Get the name of the right monitor
rightmon=$(swaymsg -t get_outputs -r | jq -r '.[] | select(.rect.x > 0 and .active == true) | .name')

# Write to the monitors config file
echo "set \$firstmon $leftmon" > "$monitors_config"
echo "set \$secondmon $rightmon" >> "$monitors_config"
