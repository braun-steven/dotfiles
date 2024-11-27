#!/usr/bin/env sh

# Symlink theme and background (-f overwrites existing symlink)
ln -f -s ~/.config/sway/background-light ~/.config/sway/background
ln -f -s ~/.config/sway/theme-light ~/.config/sway/theme
swaymsg reload

# Alacritty
sed -i --follow-symlinks 's/doom_one/alabaster/g' ~/.config/alacritty/alacritty.toml

# Vim (This is now handled in .vimrc directly)
sed -i --follow-symlinks 's/background=dark/background=light/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Adwaita
gsettings set org.gnome.desktop.wm.preferences theme Adwaita
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-light
