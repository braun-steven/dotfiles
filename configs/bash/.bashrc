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

source ~/.bash_exports

###############
# EXPORTS END #
###############

##################################
#  MISC begin                    #
##################################

[ -f ~/.conda/etc/profile.d/conda.sh ] && source ~/.conda/etc/profile.d/conda.sh


############
# MISC END #
############

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Eval keychain only locally
# NOTE: This needs to be done after the interactive if-statement
# if [[ -z $SSH_CONNECTION ]]; then
  # eval $(keychain --eval --quiet id_rsa id_ed25519)
# fi


# Start tmux in ssh connections
if [[ $SSH_CONNECTION ]]; then

# Try to attach to the ssh_tmux session, else create one
  if [[ $- =~ i ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_TTY" ]]; then
    tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
  fi
fi

# Source aliases finally
source ~/.bash_aliases

# Go into zsh
if [[ $(ps --no-header --pid=$PPID --format=cmd) != "zsh" ]] && [[ -z $SSH_CONNECTION ]]; then
  exec zsh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

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
