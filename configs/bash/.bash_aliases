alias python="python3"
alias grep="grep --color=auto"
alias eZ="$EDITOR ~/.zshrc"
alias rZ="source ~/.zshrc"
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
# alias rsync="rsync --archive --compress-level=3 --copy-links --partial --inplace --info=progress2 --rsh=ssh -r"
alias rsync="rsync --archive --compress-level=3 --copy-links --partial --inplace --rsh=ssh -r"

# Update all pip packages
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

# Emacsclient terminal
alias em="emacsclient -t -a ''"

# Better ls
if hash exa 2>/dev/null; then
  alias ls='exa -l --group-directories-first --color auto'
else
  alias ls='ls -lh --color=auto --group-directories-first'
fi

# Quicklook
alias ql="qlmanage -p"
