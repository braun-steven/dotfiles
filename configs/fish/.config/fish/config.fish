if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Aliases
alias python="python3"
alias grep="grep --color=auto"
# alias eZ="$EDITOR ~/.zshrc"
# alias rZ="source ~/.zshrc"
alias reboot="sudo systemctl reboot"
alias poweroff="sudo systemctl poweroff"
alias :q="exit"
alias vimupdate="vim +PlugClean +PlugUpdate +UpdateRemoteRepositories +qa"
alias i3config="$EDITOR ~/.config/i3/config"
# alias rsync="rsync --archive --compress-level=3 --copy-links --partial --inplace --info=progress2 --rsh=ssh -r"
alias rsync="rsync --archive --compress-level=3 --copy-links --partial --inplace --rsh=ssh -r"

# Emacsclient terminal
alias em="emacsclient -t -a ''"

# Better ls
if type -q exa
  alias ls='exa -l --group-directories-first --color auto'
else
  alias ls='ls -lh --color=auto --group-directories-first'
end

##################################
#  EXPORTS begin                 #
##################################


# Check if nvim is available
if type -q nvim 
  # Use nvim for manpages
  # set -x MANPAGER="nvim -c 'set ft man' -"
  # Emacsclient as (sudo-)editor
  set -x EDITOR "nvim"
  set -x SUDO_EDITOR "nvim"
  alias vim=nvim
  set -x SYSTEMD_EDITOR "nvim"
else
  set -x EDITOR "vim"
  set -x SUDO_EDITOR "vim"
  set -x SYSTEMD_EDITOR "vim"
end


# FZF options
set -x FZF_DEFAULT_OPTS '--height 40% --border'
set -x FZF_DEFAULT_COMMAND 'ag -g .'

# Fixes some terminal application colors
set -x TERM "xterm-256color"

# extend $PATH
set -x PATH "$PATH:$HOME/.local/bin"
set -x PATH "$PATH:$HOME/.cargo/bin"
set -x PATH "$PATH:$HOME/dotbin"  # scripts from dotfiles
set -x PATH "$HOME/bin:$PATH"  # local binaries
set -x PATH "$PATH:$HOME/.emacs.d/bin" # doom binaries
set -x PATH "$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"  # yarn
set -x PATH "$PATH:/opt/homebrew/bin"  # homebrew

set -x LD_LIBRARY_PATH "$LD_LIBRARY_PATH:/usr/lib/"


##################################
#  EXPORTS end                   #
##################################

# Enable direnv
direnv hook fish | source

##########################
# Keybindings in vi-mode #
##########################
bind -M insert \cp history-search-backward
bind -M insert \cn history-search-forward
bind -M insert \cf forward-char
bind -k sright forward-bigword


#########
# Theme #
#########

# Colorscheme: Base 16
set -U fish_color_normal normal
set -U fish_color_command a1b56c
set -U fish_color_quote f7ca88
set -U fish_color_redirection d8d8d8
set -U fish_color_end ba8baf
set -U fish_color_error ff5f00
set -U fish_color_param d8d8d8
set -U fish_color_comment f7ca88
set -U fish_color_match 7cafc2
set -U fish_color_selection white --bold --background=brblack
set -U fish_color_search_match bryellow --background=brblack
set -U fish_color_history_current --bold
set -U fish_color_operator 7cafc2
set -U fish_color_escape 86c1b9
set -U fish_color_cwd green
set -U fish_color_cwd_root red
set -U fish_color_valid_path --underline
set -U fish_color_autosuggestion 585858
set -U fish_color_user brgreen
set -U fish_color_host normal
set -U fish_color_cancel --reverse
set -U fish_pager_color_background
set -U fish_pager_color_prefix normal --bold --underline
set -U fish_pager_color_progress brwhite --background=cyan
set -U fish_pager_color_completion normal
set -U fish_pager_color_description B3A06D
set -U fish_pager_color_selected_background --background=brblack
set -U fish_pager_color_selected_prefix
set -U fish_pager_color_selected_completion
set -U fish_pager_color_selected_description
set -U fish_pager_color_secondary_background
set -U fish_color_host_remote
set -U fish_pager_color_secondary_description
set -U fish_color_option
set -U fish_pager_color_secondary_prefix
set -U fish_color_keyword
set -U fish_pager_color_secondary_completion
