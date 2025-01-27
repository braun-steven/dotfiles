#!/bin/bash - 
#===============================================================================
#
#          FILE: update.sh
# 
#         USAGE: ./update.sh 
# 
#   DESCRIPTION: Update the dotfiles repository 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Steven Braun 
#  ORGANIZATION: 
#       CREATED: 12/29/2018 10:13
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

git add . -v
git commit -m "Update dotfiles"
git push
