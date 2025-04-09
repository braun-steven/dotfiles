#!/usr/bin/env sh

# Symlink sway theme and background (-f overwrites existing symlink)
# ln -f -s ~/.config/sway/theme-dark ~/.config/sway/theme
# ln -f -s ~/.config/sway/background-dark ~/.config/sway/background
# swaymsg reload

# Alacritty
# sed -i --follow-symlinks 's/alabaster/doom_one/g' ~/.config/alacritty/alacritty.toml

# Vim (This is now handled in .vimrc directly)
# sed -i --follow-symlinks 's/background=light/background=dark/g' ~/.vimrc

# Gnome
# gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark
# gsettings set org.gnome.desktop.wm.preferences theme Adwaita-dark
# gsettings set org.gnome.desktop.interface icon-theme Adwaita
# gsettings set org.gnome.desktop.interface cursor-theme Adwaita
# gsettings set org.gnome.desktop.interface color-scheme prefer-dark

gsettings set org.gnome.desktop.interface gtk-theme Colloid-Dark
gsettings set org.gnome.desktop.wm.preferences theme Colloid-Dark
gsettings set org.gnome.desktop.interface icon-theme Colloid
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
