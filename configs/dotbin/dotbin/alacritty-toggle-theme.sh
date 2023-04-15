#!/bin/bash

# Check if an argument is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 [light|dark]"
    exit 1
fi

# Define the config file path
CONFIG_FILE="$HOME/.config/alacritty/alacritty.yml"

# Check if the config file exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "Config file not found at $CONFIG_FILE"
    exit 1
fi

# Perform the sed replacement based on the argument
if [ "$1" == "light" ]; then
    sed -i 's/doom_one.yaml/papertheme.yaml/g' "$CONFIG_FILE"
elif [ "$1" == "dark" ]; then
    sed -i 's/papertheme.yaml/doom_one.yaml/g' "$CONFIG_FILE"
else
    echo "Invalid argument. Use 'light' or 'dark'."
    exit 1
fi

echo "Theme changed to $1."
