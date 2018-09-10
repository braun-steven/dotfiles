#!/bin/bash

if [ ! -e /bin/zsh ]
then
	echo "Installing zsh"
	sudo pacman -S zsh
fi

# Install oh-my-zsh
echo "Installing oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

source ~/.zshrc

echo "Installing vim-plug"
sudo pacman -S vim neovim vim-plug vim-airline google-chrome rankmirrors 

echo "Installing plugins"
vim +PlugInstall 

# Installing some software
sudo pacman -S gnome-screenshot
sudo cp scripts/gscrot /usr/bin/gscrot
sudo chmod +x /usr/bin/gscrot

echo "Finished"
