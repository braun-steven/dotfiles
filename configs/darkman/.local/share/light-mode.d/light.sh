#!/usr/bin/env sh

# Sway
swaymsg "output * bg ~/lakesidedeer-light.png fill"
sed -i 's/theme-dark/theme-light/g' ~/.config/sway/config
swaymsg reload

# Alacritty
sed -i 's/nord/github_light/g' ~/.config/alacritty/alacritty.toml

# Vim
sed -i 's/background=dark/background=light/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Adwaita
gsettings set org.gnome.desktop.wm.preferences theme Adwaita
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-light
