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

# Check if Zellij is installed
if ! command -v zellij &> /dev/null
then
    echo "Checking for Zellij compatibility..."

    # Check system architecture
    ARCH=$(uname -m)
    case "$ARCH" in
        x86_64)
            BINARY_TAG="x86_64-unknown-linux-musl"
            ;;
        aarch64)
            BINARY_TAG="aarch64-unknown-linux-musl"
            ;;
        *)
            # Unsupported architecture, do nothing
            echo "Unsupported architecture: $ARCH. Zellij installation is skipped."
            exit 0
            ;;
    esac

    echo "Installing Zellij..."
    # Create ~/dotbin if it doesn't exist
    mkdir -p ~/dotbin

    # Navigate to ~/dotbin
    cd ~/dotbin

    # Download the latest Zellij binary for the detected architecture
    wget $(curl -s https://api.github.com/repos/zellij-org/zellij/releases/latest | grep 'browser_download_url' | grep "$BINARY_TAG" | cut -d '"' -f 4)

    # Extract the downloaded tar file
    tar -xvf zellij*.tar.gz

    # Ensure Zellij is executable
    chmod +x zellij

    # Cleanup downloaded tar file
    rm zellij*.tar.gz

    echo "Zellij installed successfully."
fi


# Download and install fzf
if [[ ! -d $HOME/.fzf ]]; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
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

# # Try to attach to the ssh_tmux session, else create one
#   if [[ $- =~ i ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_TTY" ]]; then
#     tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
#     exit  # Exit afterward
#   fi
# fi

# Start Zellij in ssh connections
if [[ $SSH_CONNECTION ]]; then

  # Try to attach to the ssh_zellij session, else create one
  if [[ $- =~ i ]] && [[ -z "$ZELLIJ" ]] && [[ -n "$SSH_TTY" ]]; then
    zellij attach ssh_zellij || zellij -s ssh_zellij
    exit  # Exit afterward
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
