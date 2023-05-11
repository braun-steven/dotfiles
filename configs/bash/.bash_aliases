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
alias ql="qlmanage -p"

# Emacsclient terminal
alias em="emacsclient -t -a ''"


# Better ls
# if hash exa 2>/dev/null; then
#   alias ls='exa -l --group-directories-first --color auto'
# fi
