#!/bin/bash - 
#===============================================================================
#
#          FILE: setup-config-symlinks.sh
# 
#         USAGE: ./setup-config-symlinks.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#  ORGANIZATION: 
#       CREATED: 01/09/2019 22:08
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

function link {
	name=$1
	from=$2
	to=$3

	if[ -e $1 ]
	then
		echo "Existing $1 config found. Creating backup with .bak extension ..."
		mv $3 $3.bak
	fi
	ln -s $2 $3
	echo "Created symlink:"
	echo "$2 -> $3"
}

# Setup links 
link "zsh" ../config/zsh/.zshrc ~/.zshrc
link "i3" ../config/i3 ~/.config/i3
link "i3status" ../config/i3status ~/.config/i3status
link "vim" ../config/vim/.vimrc ~/.vimrc
link "polybar" ../config/polybar ~/.config/polybar
link "flake8" ../config/flake/flake8 ~/.flake8
