#!/bin/bash

# SPACE_ICONS=("1 󰖟 web" "2 󰆍 main" "3 󰞷 sub" "4" "5" "6" "7" "8  spotify" "9 󰊫 mail+cal" "10 󰻞 chat" "11 (y) LLms" "12 (u)" "13 (i)" "14 (o)" "15  (p) gtd")
SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11 y" "12 u" "13 i" "14 o" "15 p")

# Destroy space on right click, focus space on left click.
# New space by left clicking separator (>)

sid=0
spaces=()
for i in "${!SPACE_ICONS[@]}"
do
  sid=$(($i+1))

  space=(
    space=$sid
    padding_left=2
    padding_right=2
    icon="${SPACE_ICONS[i]}"
    icon.padding_left=10
    icon.padding_right=5
    icon.color=$WHITE
    icon.highlight_color=$WHITE
    label.padding_right=20
    label.color=$WHITE
    label.highlight_color=$WHITE
    label.font="sketchybar-app-font:Regular:12.0"
    label.y_offset=-1
    background.color=$TRANSPARENT
    background.border_color=$BACKGROUND_2
    background.border_width=1
    script="$PLUGIN_DIR/space.sh"
  )

  sketchybar --add space space.$sid left    \
             --set space.$sid "${space[@]}" \
             --subscribe space.$sid mouse.clicked
done

space_creator=(
  icon=􀆊
  icon.font="$FONT:Heavy:16.0"
  padding_left=10
  padding_right=8
  label.drawing=off
  display=active
  click_script='yabai -m space --create'
  script="$PLUGIN_DIR/space_windows.sh"
  icon.color=$WHITE
)

sketchybar --add item space_creator left               \
           --set space_creator "${space_creator[@]}"   \
           --subscribe space_creator space_windows_change
