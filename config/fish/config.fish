set fish_greeting


# Aliases
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias cat='ccat'

# Better ls
alias ls='ls -lh --color=auto --group-directories-first'
alias grep='grep --color=auto'

# Pacman shortcuts
alias pacu='sudo pacman -Syyu && yay -Syyua'
alias pacs='sudo pacman -S'
alias yay='yay --noconfirm'
# Moved to update functions (see above)

# zshrc editing
alias eZ='vim ~/.zshrc'
alias rZ='source ~/.zshrc'
alias reboot='sudo systemctl reboot'
alias poweroff='sudo systemctl poweroff'
# Grub update - (currently systemd-boot -> no need for that)
#alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# i3-shortcuts
alias i3config='vim ~/.config/i3/config'
alias i3statusconfig='vim ~/.config/i3status/config'

# Fast terminal-directory navigation
alias xclip='xclip -selection c'
alias PWD='echo (pwd) | xclip && pwd && echo "path copied"'
alias CD='echo "cd (xclip -o)" && cd (xclip -o)'
alias :q='exit'

alias img='feh'
alias pdf='zathura'

alias vimconfig='vim ~/dotfiles/config/vim/.vimrc'
alias vimupdate='vim +PlugClean +PlugUpdate +UpdateRemoteRepositories +qa'
alias zshconfig='vim ~/dotfiles/config/zsh/.zshrc'
alias zshreload='source ~/.zshrc'
alias i3config='vim ~/dotfiles/config/i3/config'
alias xresourcesconfig='vim ~/.Xresources'
alias xresourcesreload='xrdb -merge ~/.Xresources'

alias gnome-screenshot='gnome-screenshot -a'
alias vim=nvim

alias envactivate='source ./env/bin/activate'
