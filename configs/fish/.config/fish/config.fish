if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Exports can be found in .bashrc from which we start the fish shell
# Aliases can be found in .bash_aliases which is sourced in .bashrc from which we start the fish shell

# Enable direnv
direnv hook fish | source

##########################
# Keybindings in vi-mode #
##########################
bind -M insert \cp history-search-backward
bind -M insert \cn history-search-forward
bind -M insert \cf forward-char


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

# Load aliases
source ~/.bash_aliases

# Better ls
if type -q exa
  alias ls='exa -l --group-directories-first --color auto'
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f $HOME/.conda/bin/conda
    eval $HOME/.conda/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

