alias python="python3"
alias pip="pip3"
alias grep="grep --color=auto"
alias eZ="$EDITOR ~/.zshrc"
alias rZ="source ~/.zshrc"
alias eF="$EDITOR ~/.config/fish/config.fish"
alias rF="source ~/.config/fish/config.fish"
alias reboot="sudo systemctl reboot"
alias poweroff="sudo systemctl poweroff"
alias i3config="$EDITOR ~/.config/i3/config"
alias swayconfig="$EDITOR ~/.config/sway/config"
alias clone="termite -e 'bash' 2>&1 >/dev/null & disown %1"
alias :q="exit"
alias vimconfig="$EDITOR ~/.vimrc"
alias vimupdate="vim +PlugClean +PlugUpdate +UpdateRemoteRepositories +qa"
alias zshconfig="$EDITOR ~/.zshrc"
alias zshreload="source ~/.zshrc"
alias i3config="$EDITOR ~/.config/i3/config"
alias xresourcesconfig="$EDITOR ~/.Xresources"
alias xresourcesreload="xrdb -merge ~/.Xresources"
alias rsync="rsync --archive --compress-level=3 --copy-links --partial --inplace --rsh=ssh -r"
alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

# Emacsclient terminal
alias em="emacsclient -t -a ''"

# DNF on Fedora
alias dnfi="sudo dnf install"
alias dnfr="sudo dnf remove"
alias dnfs="dnf search"
alias dnfu="echo '<<<<<<<<<<<<<<< Updating dnf' && sudo dnf update && echo -e '\n\n<<<<<<<<<<<<<<< Updating flatpaks' && flatpak update" # && echo -e '\n\n<-[Updating snaps]->' && sudo snap refresh"

alias ls='ls -lh --group-directories-first'


# TUVPN
alias tuvpn="sudo tuvpn"
