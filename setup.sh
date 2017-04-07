#!/bin/bash

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "Linking .zshrc"
# Link .zshrc
if [ -e ~/.zshrc ]
then
	echo "Created backup ~/.zshrcbak"
	mv ~/.zshrc ~/.zshrcbak
fi
ln -s ./zsh/.zshrc ~/.zshrc

# Link .vimrc
echo "Linking .vimrc"
if [ -e ~/.vimrc ]
then
	echo "Created backup ~/.vimrc"
	mv ~/.vimrc ~/.vimrcbak
fi
ln -s ./vim/.vimrc ~/.vimrc
echo "Keep in mind to install Vundle and execute ':PluginInstall' in vim\n"

# Link i3
echo "Linking i3 configs"
if [ -e ~/.config/i3/config ]
then
	echo "Created backup ~/.config/i3/configbak"
	mv ~/.config/i3/config ~/.config/i3/configbak
fi
if [ -e ~/.i3/config ]
then
	echo "Created backup ~/.i3/configbak"
	mv ~/.i3/config ~/.i3/configbak
	mkdir .config/i3
fi
ln -s ./i3/config ~/.config/i3/config


if [ -e ~/.config/i3status/config ]
then
	echo "Created backup ~/.config/i3status/configbak"
	mv ~/.config/i3status/config ~/.config/i3status/configbak
fi
ln -s .i3status/config ~/.config/i3status/config

echo "Finished"
