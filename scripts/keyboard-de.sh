#!/bin/bash - 
#===============================================================================
#
#          FILE: keyboard-de.sh
# 
#         USAGE: ./keyboard-de.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: YOUR NAME (), 
#  ORGANIZATION: 
#       CREATED: 03/06/2019 10:44
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
setxkbmap de -option caps:escape

