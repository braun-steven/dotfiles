#!/usr/bin/env sh

# Sway
swaymsg "output * bg ~/lakesidedeer-dark.png fill"
sed -i --follow-symlinks 's/theme-light/theme-dark/g' ~/.config/sway/config
swaymsg reload

# Alacritty
sed -i --follow-symlinks 's/github_light/nord/g' ~/.config/alacritty/alacritty.toml

# Vim
sed -i --follow-symlinks 's/background=light/background=dark/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark
gsettings set org.gnome.desktop.wm.preferences theme Adwaita-dark
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
