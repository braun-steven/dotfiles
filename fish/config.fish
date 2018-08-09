alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias cat='ccat'

# Better ls
alias ls='ls -lh --color=auto --group-directories-first'
alias grep='grep --color=auto'

# Pacman shortcuts
alias pacu='sudo pacman -Syyu; and yay -Syyua'
alias pacs='sudo pacman -S'
alias pacrsc='sudo pacman -Rsc'
alias yay='yay --noconfirm'
# Moved to update functions (see above)
#alias pacu='sudo pacman -Syu; and yay -Syua'
#alias pacufull='sudo pacman -Syyu; and yay -Syua'

alias reboot='sudo systemctl reboot'
alias poweroff='sudo systemctl poweroff'
# Grub update - (currently systemd-boot -> no need for that)
#alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# i3-shortcuts
alias i3config='vim ~/.config/i3/config'
alias i3statusconfig='vim ~/.config/i3status/config'

# Fast terminal-directory navigation
alias xclip='xclip -selection c'
alias PWD='echo (pwd) | xclip; and pwd; and echo "path copied"'
alias CD='echo "cd (xclip -o)"; and cd (xclip -o)'
alias :q='exit'

alias img='feh'
alias pdf='zathura'

alias edit-vim='vim ~/dotfiles/vim/.vimrc'
alias edit-zsh='vim ~/dotfiles/zsh/.zshrc'
alias edit-i3='vim ~/dotfiles/i3/config'
alias edit-i3status='vim ~/dotfiles/i3status/config'

alias ga='git add -v'
alias gaa='git add -A -v'
alias gaad='git add -A -v --dry-run'
alias gc='git commit'
alias gcm='git commit -m'

alias qutebrowser='qutebrowser --backend webengine'

alias gnome-screenshot='gnome-screenshot -a'

alias rpi='ssh rpi'

export JAVA_HOME=/usr/lib/jvm/java-8-jdk
export TERMINAL=gnome-terminal
export EDITOR=vim
export VISUAL=vim

# Maven java server debugging
export MAVEN_OPTS=-agentlib:jdwp=transport=dt_socket,address=8000,server=y,suspend=n
set PATH (ruby -e 'print Gem.user_dir')/bin $PATH
set PATH /home/tak/bin $PATH
set LD_LIBRARY_PATH $LD_LIBRARY_PATH /usr/lib/

keychain --eval --quiet id_rsa id_rsa_tak3r07 id_rsa_mz > /dev/null

# Base16 Shell
set BASE16_SHELL "$HOME/.config/base16-shell/"
[ -n "$PS1" ]; and \
[ -s "$BASE16_SHELL/profile_helper.sh" ] ; and \
eval "$BASE16_SHELL/profile_helper.sh" 

function gi 
	curl -L -s https://www.gitignore.io/api/$argv ;
end

