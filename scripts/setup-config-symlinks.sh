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

	if [[ -L "$3" ]] || [[ -f "$3" ]]; then
		echo "Existing $1 config found. Creating backup with .bak extension ..."
		mv $3 $3.bak
	fi
	ln -s $2 $3
	echo "Created symlink:"
	echo "$2 -> $3"
}

dotfiles_dir="~/dotfiles"

# Setup links 
link "zsh" $dotfiles_dir/config/zsh/.zshrc ~/.zshrc
link "i3" $dotfiles_dir/config/i3 ~/.config/i3
link "i3status" $dotfiles_dir/config/i3status ~/.config/i3status
link "vim" $dotfiles_dir/config/vim/.vimrc ~/.vimrc
link "polybar" $dotfiles_dir/config/polybar ~/.config/polybar
link "flake8" $dotfiles_dir/config/flake/flake8 ~/.flake8
