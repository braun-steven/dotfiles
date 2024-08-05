#!/usr/bin/env sh

# Sway
# sed -i --follow-symlinks 's/theme-light/theme-dark/g' ~/.config/sway/config
# swaymsg reload
# swaymsg "output * bg ~/lakesidedeer-dark.png fill" # Set background after sway reload

# Alacritty
# sed -i --follow-symlinks 's/github_light/nord/g' ~/.config/alacritty/alacritty.toml

# Vim (This is now handled in .vimrc directly)
# sed -i --follow-symlinks 's/background=light/background=dark/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Colloid-Nord
gsettings set org.gnome.desktop.wm.preferences theme Colloid-Nord
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
