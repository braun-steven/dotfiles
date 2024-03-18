if status is-interactive
    # Commands to run in interactive sessions can go here
end


# Exports can be found in .bashrc from which we start the fish shell
# Aliases can be found in .bash_aliases which is sourced in .bashrc from which we start the fish shell

##########################
# Keybindings in vi-mode #
##########################

# Enable fish vi mode
fish_vi_key_bindings

bind -M insert \cp up-or-search
bind -M insert \cn down-or-search
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

# If nvim is available, replace vim with nvim
if type -q nvim
  alias vim=nvim
end

# Better ls
if type -q eza
  alias ls='exa -l --group-directories-first --color auto'
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

# Define maybe-activate-conda-env on variable change of PWD
function __maybe_activate_conda_env --on-variable PWD
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
