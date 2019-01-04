#!/bin/bash - 
#===============================================================================
#
#          FILE: launch.sh
# 
#         USAGE: ./launch.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: YOUR NAME (), 
#  ORGANIZATION: 
#       CREATED: 01/03/2019 14:07
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
polybar example &

echo "Bars launched..."

