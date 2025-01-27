#
# ~/.bashrc
#

source ~/.bash_exports

#############################
# INSTALLING BINARIES BEGIN #
#############################

# Check if tpm is installed
if [ ! -d $HOME/.tmux/plugins/tpm ]; then
  echo "Tmux plugin manager not found. Installing now ..."
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi


# Check if fzf is available, if not, check if ~/.fzf exists, and if neither, clone and install
if ! command -v fzf &> /dev/null; then
  if [[ ! -d $HOME/.fzf ]]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  fi
  ~/.fzf/install --key-bindings --completion --update-rc
fi


# Check if zoxide is installed
if ! command -v zoxide &> /dev/null; then
  echo "Zoxide not found. Installing now ..."
  curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh
fi


###########################
# INSTALLING BINARIES END #
###########################


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Eval keychain only locally
# NOTE: This needs to be done after the interactive if-statement
if [[ -z $SSH_CONNECTION ]]; then
  if command -v keychain &> /dev/null; then
    eval $(keychain --eval --quiet id_rsa id_ed25519)
  fi
fi



# # Start tmux in ssh connections
# if [[ $SSH_CONNECTION ]]; then

# Start Zellij or tmux in SSH connections
if [[ $SSH_CONNECTION ]]; then
  if [[ $- =~ i ]] && [[ -n "$SSH_TTY" ]]; then
    # Run tmux if available
    if command -v tmux &> /dev/null; then
      # Try to attach to the ssh_tmux session, else create one
      if [[ -z "$TMUX" ]]; then
        tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
        exit  # Exit afterward
      fi
    else
      echo "Tmux is not available"
    fi
  fi
fi


source ~/.bash_aliases

# Use Fish if available, otherwise fall back to Zsh
if command -v fish &> /dev/null; then
    # if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" ]]; then
    #     exec fish
    # fi

    # Avoid creating an infinite loop of shells
    if [[ $(uname) == "Darwin" ]]; then  # MacOS (has different ps syntax)
      if [[ $(ps -o comm= -p $PPID) != "fish" ]]; then
        exec fish
      fi
    else  # Linux
      if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" ]]; then
        exec fish
      fi
    fi
elif command -v zsh &> /dev/null; then
    exec zsh
fi

# . "$HOME/.cargo/env"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
