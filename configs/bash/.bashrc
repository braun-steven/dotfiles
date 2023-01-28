#
# ~/.bashrc
#
# Go into zsh shell

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
else
  export EDITOR="vim"
  export SUDO_EDITOR="vim"
fi


# Extend $PATH
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/dotbin"  # scripts from dotfiles
export PATH="$HOME/bin:$PATH"  # local binaries
export PATH="$PATH:$HOME/.emacs.d/bin" # doom binaries
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"  # yarn
export PATH="$PATH:/opt/homebrew/bin"  # homebrew
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/"

if [[ ! -z $SSH_CONNECTION ]]; then
  export DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"
fi


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Eval keychain only locally
# NOTE: This needs to be done after the interactive if-statement
if [[ -z $SSH_CONNECTION ]]; then
  eval $(keychain --eval --quiet id_rsa id_ed25519)
  # eval `ssh-agent`
fi

# Go into zsh
if [[ $(ps --no-header --pid=$PPID --format=cmd) != "zsh" ]] && [[ -z $SSH_CONNECTION ]]; then
  exec zsh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/steven/.conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/steven/.conda/etc/profile.d/conda.sh" ]; then
        . "/home/steven/.conda/etc/profile.d/conda.sh"
    else
        export PATH="/home/steven/.conda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

