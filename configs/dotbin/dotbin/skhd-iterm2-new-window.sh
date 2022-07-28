#!/usr/bin/env bash

# Source: https://github.com/koekeishiya/skhd/issues/20

set -e

osascript - <<EOF
tell application "iTerm2"
    set newSession to create window with default profile
end tell
EOF
