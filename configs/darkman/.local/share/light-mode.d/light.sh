#!/usr/bin/env sh

# Sway
sed -i --follow-symlinks 's/theme-dark/theme-light/g' ~/.config/sway/config
swaymsg reload
# swaymsg "output * bg ~/lakesidedeer-light.png fill" # Set background after sway reload

# Alacritty
sed -i --follow-symlinks 's/nord/github_light/g' ~/.config/alacritty/alacritty.toml

# Vim (This is now handled in .vimrc directly)
sed -i --follow-symlinks 's/background=dark/background=light/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Adwaita
gsettings set org.gnome.desktop.wm.preferences theme Adwaita
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-light
