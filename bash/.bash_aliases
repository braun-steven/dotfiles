alias python="python3"
alias grep="grep -i --color=auto"
alias reboot="sudo systemctl reboot"
alias poweroff="sudo systemctl poweroff"
alias vimupdate="vim +PlugClean +PlugUpdate +UpdateRemoteRepositories +qa"
alias rsync="rsync --progress -vh --archive --compress-level=3 --copy-links --partial --inplace --rsh=ssh -r"
# alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

# Emacsclient terminal
alias em="emacsclient -t -a ''"

# DNF on Fedora
alias dnfi="sudo dnf install"
alias dnfr="sudo dnf remove"
alias dnfs="dnf search"
alias dnfu="echo -e '\033[1;32m[sudo dnf update]\033[0m' && sudo dnf update && echo -e '\n\n\033[1;32m[flatpaks update]\033[0m' && flatpak update"

# Zypper on OpenSUSE Tumbleweed
alias ls='ls -lh --group-directories-first'

# TUVPN
alias tuvpn="sudo tuvpn"
alias lazygit='lazygit --use-config-file="$HOME/.config/lazygit/config.yml"'
