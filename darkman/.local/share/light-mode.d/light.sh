#!/usr/bin/env sh

# Symlink theme and background (-f overwrites existing symlink)
# ln -f -s ~/.config/sway/background-light ~/.config/sway/background
# ln -f -s ~/.config/sway/theme-light ~/.config/sway/theme
ln -f -s ~/.config/sway/background-light ~/.config/sway/background
swaymsg "$(cat ~/.config/sway/background-light)"
# swaymsg reload

# Alacritty
# sed -i --follow-symlinks 's|~/.config/alacritty/themes/themes/doom_one.toml|~/.config/alacritty/modus-operandi-tinted-theme.toml|g' ~/.config/alacritty/alacritty.toml

# Vim (This is now handled in .vimrc directly)
# sed -i --follow-symlinks 's/background=dark/background=light/g' ~/.vimrc

# Gnome
gsettings set org.gnome.desktop.interface gtk-theme Adwaita
gsettings set org.gnome.desktop.wm.preferences theme Adwaita
gsettings set org.gnome.desktop.interface icon-theme Adwaita
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-light

gsettings set org.gnome.desktop.interface gtk-theme Colloid-Light
gsettings set org.gnome.desktop.wm.preferences theme Colloid-Light
gsettings set org.gnome.desktop.interface icon-theme Colloid
gsettings set org.gnome.desktop.interface cursor-theme Adwaita
gsettings set org.gnome.desktop.interface color-scheme prefer-light
