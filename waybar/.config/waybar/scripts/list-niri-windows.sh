#!/usr/bin/env bash

# Get the current focused workspace ID
current_workspace=$(niri msg --json workspaces | jq '.[] | select(.is_focused==true) | .id')

# Get windows on the current workspace
windows=$(niri msg --json windows | jq --arg ws "$current_workspace" '.[] | select(.workspace_id == ($ws|tonumber))')

# Directories to search for .desktop files
desktop_dirs=(
  "$HOME/.local/share/applications"
  "/usr/share/applications"
)

# Cache for resolved names
declare -A name_cache

# Function to resolve app_id to human-readable name
resolve_name() {
  local app_id="$1"
  if [[ -n "${name_cache[$app_id]}" ]]; then
    echo "${name_cache[$app_id]}"
    return
  fi

  local desktop_file=""
  local name=""

  # Match by StartupWMClass
  for dir in "${desktop_dirs[@]}"; do
    [[ ! -d "$dir" ]] && continue
    desktop_file=$(grep -ilR "StartupWMClass=${app_id}" "$dir" 2>/dev/null | head -n1)
    [[ -n "$desktop_file" ]] && break
  done

  # Fallback: match filename
  if [[ -z "$desktop_file" ]]; then
    for dir in "${desktop_dirs[@]}"; do
      [[ ! -d "$dir" ]] && continue
      desktop_file=$(find "$dir" -maxdepth 1 -iname "${app_id}.desktop" 2>/dev/null | head -n1)
      [[ -n "$desktop_file" ]] && break
    done
  fi

  # Extract Name field
  if [[ -n "$desktop_file" ]]; then
    name=$(grep -m1 '^Name=' "$desktop_file" | cut -d= -f2-)
  fi

  # Fallback to app_id
  [[ -z "$name" ]] && name="$app_id"
  name_cache[$app_id]="$name"
  echo "$name"
}

# Build list of names with brackets for focused window
list=$(echo "$windows" | jq -r '. | "\(.app_id) \(.is_focused)"' | while read -r line; do
  app_id=$(echo "$line" | awk '{print $1}')
  focused=$(echo "$line" | awk '{print $2}')
  name=$(resolve_name "$app_id")
  [[ "$focused" == "true" ]] && name="[$name]"
  echo " $name "
done | paste -sd " " -)

# Output JSON for Waybar
echo ${list}
