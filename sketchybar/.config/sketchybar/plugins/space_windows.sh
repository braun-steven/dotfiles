#!/bin/bash

if [ "$SENDER" = "space_windows_change" ]; then
  args=(--animate sin 10)

  space="$(echo "$INFO" | jq -r '.space')"
  apps="$(echo "$INFO" | jq -r '.apps | keys[]')"
  echo "$INFO" >> /tmp/sketchybar.log

  icon_strip=" "

  # Check if there are any apps in the current space
  if [ "${apps}" != "" ]; then
    # Loop through each app
    while read -r app
    do
      # Remove leading/trailing whitespace and the Left-to-Right Mark (LRM)
      app_clean=$(xargs <<< "$app" | sed 's/\xE2\x80\x8E//g')

      # Get the icon for the cleaned app name from icon_map.sh and append to icon_strip
      icon_strip+=" $($CONFIG_DIR/plugins/icon_map.sh "$app_clean")"
    done <<< "${apps}"
  else
    # If no apps, set icon_strip to a separator
    icon_strip=" —"
  fi
  args+=(--set space.$space label="$icon_strip")

  sketchybar -m "${args[@]}"
fi
