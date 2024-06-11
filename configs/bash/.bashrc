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


# Download and install fzf
if [[ ! -d $HOME/.fzf ]]; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
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
  eval $(keychain --eval --quiet id_rsa id_ed25519)
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

# Go into zsh
# if [[ $(ps --no-header --pid=$PPID --format=cmd) != "zsh" ]] && [[ -z $SSH_CONNECTION ]]; then
if [[ $(ps -p $PPID -o command=) != "fish" ]]; then
# if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" ]]; then
  # exec zsh
  if [[ ! -z $SSH_CONNECTION ]]; then
    # Check if fish is available via homebrew

    # Check if fish is available via homebrew
    if [[ -f $HOME/homebrew/bin/fish ]]; then
      exec $HOME/homebrew/bin/fish
    else
      echo "Fish shell via homebrew not found. Using zsh instead ..."
      exec zsh
    fi
  else
    exec fish
  fi
  exit  # Exit afterward
fi


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
. "$HOME/.cargo/env"

