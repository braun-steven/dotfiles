#!/usr/bin/env sh

PATH="$PATH:/home/steven/dotbin:/home/steven/bin:/home/steven/.local/bin"

if [ "$0" = "/etc/gdm/Xsession" -a "$DESKTOP_SESSION" = "i3" ]; then
    export $(gnome-keyring-daemon --start)
    # SSH_AGENT_PID required to stop xinitrc-common from starting ssh-agent
    export SSH_AGENT_PID=${GNOME_KEYRING_PID:-gnome}
fi
