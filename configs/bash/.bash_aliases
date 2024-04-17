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
alias ql="qlmanage -p"
alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

# Emacsclient terminal
alias em="emacsclient -t -a ''"

# DNF on Fedora
alias dnfi="sudo dnf5 install"
alias dnfr="sudo dnf5 remove"
alias dnfs="dnf5 search"
alias dnfu="echo '<-[Updating dnf]->' && sudo dnf5 update && echo -e '\n\n<-[Updating flatpaks]->' && flatpak update && echo -e '\n\n<-[Updating snaps]->' && sudo snap refresh"

alias gemma2b="~/gemma.cpp/build/gemma -- --tokenizer ~/gemma.cpp/build/tokenizer.spm --compressed_weights ~/gemma.cpp/build/2b-it-sfp.sbs --model 2b-it --verbosity 0"
alias gemma7b="~/gemma.cpp/build/gemma -- --tokenizer ~/gemma.cpp/build/tokenizer.spm --compressed_weights ~/gemma.cpp/build/7b-it-sfp.sbs --model 7b-it --verbosity 0"

alias gemma2b-repl="~/gemma.cpp/build/gemma --tokenizer ~/gemma.cpp/build/tokenizer.spm --compressed_weights ~/gemma.cpp/build/2b-it-sfp.sbs --model 2b-it"
alias gemma7b-repl="~/gemma.cpp/build/gemma --tokenizer ~/gemma.cpp/build/tokenizer.spm --compressed_weights ~/gemma.cpp/build/7b-it-sfp.sbs --model 7b-it"

# Better ls
# if hash eza 2>/dev/null; then
#   alias ls='exa -l --group-directories-first --color auto'
# fi
