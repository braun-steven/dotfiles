##!/usr/bin/env sh

test -z "$PROFILEREAD" && . /etc/profile || true 

PATH="$PATH:/home/steven/dotbin:/home/steven/bin:/home/steven/.local/bin"
XDG_DATA_DIRS="$XDG_DATA_DIRS:/home/steven/.local/share"

