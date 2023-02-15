#
# ~/.bashrc
#

#############################
# INSTALLING BINARIES BEGIN #
#############################

# Check if tpm is installed
if [ ! -d $HOME/.tmux/plugins/tpm ]; then
  echo "Tmux plugin manager not found. Installing now ..."
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Download and install fzf
if [[ ! -d $HOME/.fzf ]]; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi


###########################
# INSTALLING BINARIES END #
###########################

#################
# EXPORTS BEGIN #
#################

# Add ruby binaries to path if available
if command -v ruby &> /dev/null; then
  export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
fi


# Check if nvim is available
if command -v nvim &>/dev/null; then
  # Use nvim for manpages
  # export MANPAGER="nvim -c 'set ft=man' -"
  # Emacsclient as (sudo-)editor
  export EDITOR="nvim"
  export SUDO_EDITOR="nvim"
  alias vim=nvim
  export SYSTEMD_EDITOR="nvim"
else
  export EDITOR="vim"
  export SUDO_EDITOR="vim"
  export SYSTEMD_EDITOR="vim"
fi


# Extend $PATH
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/dotbin"  # scripts from dotfiles
export PATH="$HOME/bin:$PATH"  # local binaries
export PATH="$PATH:$HOME/.emacs.d/bin" # doom binaries
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"  # yarn
export PATH="$PATH:$HOME/homebrew/bin"  # local homebrew install
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/"

# If we have an ssh connection, export the docker host
if [[ ! -z $SSH_CONNECTION ]]; then
  export DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"
fi

###############
# EXPORTS END #
###############

##################################
#  MISC begin                    #
##################################

[ -f ~/.conda/etc/profile.d/conda.sh ] && source ~/.conda/etc/profile.d/conda.sh

function eog () {
  command eog $1 & disown
}

function emacs () {
  command emacs $1 & disown
}

function evince () {
  command evince $1 & disown
}

function pdf () {
  command evince $1 & disown
}

function maybe_activate_conda_env () {

  # Check if "conda" command is available,
  if ! command -v conda &>/dev/null; then
    return
  fi

  # Check if conda env is set
  if [ ! -z "${CONDA_DEFAULT_ENV}" ]; then
    dirname=${PWD##*/}  # Get directory name without full path

    # Check if conda env is part of current pwd (allows for PWD being a subdir)
    if [[ "${PWD}" == *"${CONDA_DEFAULT_ENV}"* ]]; then
      return
    else
      echo "Deactivating conda environment ${CONDA_DEFAULT_ENV}"
      conda deactivate
      return
    fi
  fi


  # Get directory name without full path
  dirname=${PWD##*/}
  # If directory name can be found in conda evironments, activate it!
  if grep -q $dirname <(command ls ~/.conda/envs/); then
    echo "Conda environment '$dirname' found! Activating now ..."
    conda activate $dirname
  fi
}

############
# MISC END #
############

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Eval keychain only locally
# NOTE: This needs to be done after the interactive if-statement
if [[ -z $SSH_CONNECTION ]]; then
  eval $(keychain --eval --quiet id_rsa id_ed25519)
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$HOME/.conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/.conda/etc/profile.d/conda.sh" ]; then
        . "$HOME/.conda/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/.conda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# If we are in an ssh connection and we have installed the latest version of fish via homebrew
# alias the fish command to the homebrew binary
if [[ $SSH_CONNECTION ]]; then
  if [[ -f $HOME/homebrew/bin/fish ]]; then
    alias fish="$HOME/homebrew/bin/fish"
  fi
fi



# Start tmux in ssh connections
if [[ $SSH_CONNECTION ]]; then

# Try to attach to the ssh_tmux session, else create one
  if [[ $- =~ i ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_TTY" ]]; then
    tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
  fi
fi

# Source aliases finally
source ~/.bash_aliases

# Better ls
if hash exa 2>/dev/null; then
  alias ls='exa -l --group-directories-first --color auto'
fi

# Go into fish
if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z ${BASH_EXECUTION_STRING} ]]
then
	exec fish
fi
