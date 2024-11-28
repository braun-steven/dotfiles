if status is-interactive
    # Commands to run in interactive sessions can go here
end


# Exports can be found in .bashrc from which we start the fish shell
# Aliases can be found in .bash_aliases which is sourced in .bashrc from which we start the fish shell

# Check if fisher is installed; if not, install it
if not functions -q fisher
  # Install Fisher
  set -l fisher_path ~/.config/fish/functions/fisher.fish
  if not test -f $fisher_path
    curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish -o $fisher_path
  end
end
##########################
# Keybindings in vi-mode #
##########################

# Enable fish vi mode
fish_vi_key_bindings

bind -M insert \cp up-or-search
bind -M insert \cn down-or-search
bind -M insert \cf forward-char

bind -M insert \ch backward-char
bind -M insert \cl forward-char

# Fzf
# set -U FZF_DEFAULT_OPTS "--height 40% --layout=reverse --border --prompt='Search> '"
fzf_configure_bindings --directory=\ct
set fzf_history_time_format

#########
# Theme #
#########

# Function to set the color scheme to light
function set_light_theme
  # Colorscheme: Custom Base16 Default Light
  set -U fish_color_normal normal
  set -U fish_color_command 008700
  set -U fish_color_quote af8700
  set -U fish_color_redirection 383838
  set -U fish_color_end ba8baf
  set -U fish_color_error ab4642
  set -U fish_color_param 383838
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
  set -U fish_color_autosuggestion 808080
  set -U fish_color_user brgreen
  set -U fish_color_host normal
  set -U fish_color_cancel --reverse
  set -U fish_pager_color_prefix normal --bold --underline
  set -U fish_pager_color_progress brwhite --background=cyan
  set -U fish_pager_color_completion normal
  set -U fish_pager_color_description B3A06D
  set -U fish_pager_color_selected_background --background=brblack
  set -U fish_pager_color_background
  set -U fish_pager_color_selected_description
  set -U fish_color_host_remote
  set -U fish_color_keyword
  set -U fish_pager_color_selected_prefix
  set -U fish_pager_color_secondary_prefix
  set -U fish_pager_color_secondary_background
  set -U fish_pager_color_secondary_description
  set -U fish_color_option
  set -U fish_pager_color_secondary_completion
  set -U fish_pager_color_selected_completion
end


function set_dark_theme
  # Colorscheme: Dark
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
  set -U fish_color_host_remote
  set -U fish_color_keyword
  set -U fish_pager_color_secondary_prefix
  set -U fish_pager_color_secondary_background
  set -U fish_pager_color_secondary_description
  set -U fish_color_option
  set -U fish_pager_color_secondary_completion
end

function set_theme_auto
  # Read value of darkman if available
  if type -q darkman
    set current_theme (darkman get)
  else
    set current_theme dark
  end

  # set_dark_theme
  if test $current_theme = "light"
    set_light_theme
  else
    set_dark_theme
  end
end

set_theme_auto
# set_dark_theme

# Load aliases
source ~/.bash_aliases

# If nvim is available, replace vim with nvim
if type -q nvim
  alias vim=nvim
end

# Better ls
if type -q eza
  alias ls='eza -l --group-directories-first --color auto'
end


###################
# FUNCTIONS BEGIN #
###################

function eog
  command eog $argv[1] & disown
end

function emacs
  command emacs $argv[1] & disown
end

function evince
  command evince $argv[1] & disown
end

function pdf
  command evince $argv[1] & disown
end

function initconda
  if test -f $HOME/.conda/bin/conda
      eval $HOME/.conda/bin/conda "shell.fish" "hook" $argv | source
  end
end

function dnfu
    # Get the current day of the week (1 is Monday, 7 is Sunday)
    set current_day (date +%u)

    # Check if it's Saturday (6) or Sunday (7)
    if test $current_day -eq 6 -o $current_day -eq 7
        echo -e '\033[1;32m[sudo dnf update]\033[0m'
        sudo dnf update
        echo -e '\n\n\033[1;32m[flatpaks update]\033[0m'
        flatpak update
    else
        echo -e '\033[1;31mUpdates are only allowed on weekends (Saturday and Sunday).\033[0m'
        echo -e '\033[1;31mToday is not a weekend. Update aborted.\033[0m'
    end
end


# Define maybe-activate-conda-env function
function maybe_activate_conda_env

  # Check if conda exists
  if not test -d ~/.conda; or not test -f ~/.conda/bin/conda
    return
  end

  # Check if "conda" command is available,
  if not type -q conda
     initconda
  end

  # Check if conda env is set
  # fish shell: check if $CONDA_DEFAULT_ENV is set
  if test -n "$CONDA_DEFAULT_ENV"
    set dirname (path basename $PWD)

    # Check if current conda env is no longer part of current pwd
    if not string match -q "*$CONDA_DEFAULT_ENV*" "$PWD"
      echo "Deactivating conda environment $CONDA_DEFAULT_ENV"
      conda deactivate
      return
    end
  end

  # Get directory name without full path
  set dirname (path basename $PWD)
  # If directory name can be found in conda evironments, activate it!
  if command ls ~/.conda/envs/ | grep -q $dirname
    echo "Conda environment '$dirname' found! Activating now ..."
    initconda
    conda activate $dirname
  end
end

# Run the function when PWD changes
function __on_pwd_change --on-variable PWD
  maybe_activate_conda_env
end

# Run the function when the shell starts
maybe_activate_conda_env

#################
# FUNCTIONS END #
#################


#############################
# CUSTOM AUTOCOMPLETE BEGIN #
#############################

complete -c pdf -k -xa "(__fish_complete_suffix pdf)"
complete -c evince -k -xa "(__fish_complete_suffix pdf)"
complete -c eog -k -xa "(__fish_complete_suffix jpg)"
complete -c eog -k -xa "(__fish_complete_suffix jpeg)"
complete -c eog -k -xa "(__fish_complete_suffix png)"
complete -c eog -k -xa "(__fish_complete_suffix gif)"

############################
# CUSTOM AUTOCOMPLETE END  #
############################


# Enable direnv
direnv hook fish | source

# Initialize conda
initconda

# =============================================================================
#
# Utility functions for zoxide.
#

# pwd based on the value of _ZO_RESOLVE_SYMLINKS.
function __zoxide_pwd
    builtin pwd -L
end

# A copy of fish's internal cd function. This makes it possible to use
# `alias cd=z` without causing an infinite loop.
if ! builtin functions --query __zoxide_cd_internal
    if builtin functions --query cd
        builtin functions --copy cd __zoxide_cd_internal
    else
        alias __zoxide_cd_internal='builtin cd'
    end
end

# cd + custom logic based on the value of _ZO_ECHO.
function __zoxide_cd
    __zoxide_cd_internal $argv
end

# =============================================================================
#
# Hook configuration for zoxide.
#

# Initialize hook to add new entries to the database.
function __zoxide_hook --on-variable PWD
    test -z "$fish_private_mode"
    and command zoxide add -- (__zoxide_pwd)
end

# =============================================================================
#
# When using zoxide with --no-cmd, alias these internal functions as desired.
#

if test -z $__zoxide_z_prefix
    set __zoxide_z_prefix 'z!'
end
set __zoxide_z_prefix_regex ^(string escape --style=regex $__zoxide_z_prefix)

# Jump to a directory using only keywords.
function __zoxide_z
    set -l argc (count $argv)
    if test $argc -eq 0
        __zoxide_cd $HOME
    else if test "$argv" = -
        __zoxide_cd -
    else if test $argc -eq 1 -a -d $argv[1]
        __zoxide_cd $argv[1]
    else if set -l result (string replace --regex $__zoxide_z_prefix_regex '' $argv[-1]); and test -n $result
        __zoxide_cd $result
    else
        set -l result (command zoxide query --exclude (__zoxide_pwd) -- $argv)
        and __zoxide_cd $result
    end
end

# Completions.
function __zoxide_z_complete
    set -l tokens (commandline --current-process --tokenize)
    set -l curr_tokens (commandline --cut-at-cursor --current-process --tokenize)

    if test (count $tokens) -le 2 -a (count $curr_tokens) -eq 1
        # If there are < 2 arguments, use `cd` completions.
        complete --do-complete "'' "(commandline --cut-at-cursor --current-token) | string match --regex '.*/$'
    else if test (count $tokens) -eq (count $curr_tokens); and ! string match --quiet --regex $__zoxide_z_prefix_regex. $tokens[-1]
        # If the last argument is empty and the one before doesn't start with
        # $__zoxide_z_prefix, use interactive selection.
        set -l query $tokens[2..-1]
        set -l result (zoxide query --exclude (__zoxide_pwd) --interactive -- $query)
        and echo $__zoxide_z_prefix$result
        commandline --function repaint
    end
end
complete --command __zoxide_z --no-files --arguments '(__zoxide_z_complete)'

# Jump to a directory using interactive search.
function __zoxide_zi
    set -l result (command zoxide query --interactive -- $argv)
    and __zoxide_cd $result
end

# =============================================================================
#
# Commands for zoxide. Disable these using --no-cmd.
#

abbr --erase z &>/dev/null
alias z=__zoxide_z

abbr --erase zi &>/dev/null
alias zi=__zoxide_zi

# Initialize zoxide
zoxide init fish | source
