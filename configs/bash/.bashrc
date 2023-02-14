#
# ~/.bashrc
#

# Add ruby binaries to path if available
if command -v ruby &> /dev/null; then
  PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
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

# Start tmux in ssh connections
if [[ $SSH_CONNECTION ]]; then

# Try to attach to the ssh_tmux session, else create one
  if [[ $- =~ i ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_TTY" ]]; then
    tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
  fi
fi

# Source aliases finally
source ~/.bash_aliases

# Go into fish
if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z ${BASH_EXECUTION_STRING} ]]
then
	exec fish
fi
