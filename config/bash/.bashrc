#
# ~/.bashrc
#
# Go into zsh shell
if [[ "$TERM" == *xterm* ]]; then
   exec zsh 
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
