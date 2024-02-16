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

# Define a function to download and install Zellij
install_zellij() {
    local BINARY_TAG=$1  # Pass the binary tag as an argument to the function

    echo "Installing Zellij for architecture: $BINARY_TAG..."

    # Create ~/dotbin if it doesn't exist
    mkdir -p ~/.local/bin

    # Navigate to ~/dotbin
    cd ~/.local/bin

    # Download the latest Zellij binary for the detected architecture
    wget $(curl -s https://api.github.com/repos/zellij-org/zellij/releases/latest | grep 'browser_download_url' | grep "$BINARY_TAG" | cut -d '"' -f 4)

    # Extract the downloaded tar file
    tar -xvf zellij*.tar.gz

    # Ensure Zellij is executable
    chmod +x zellij

    # Cleanup downloaded tar file
    rm zellij*.tar.gz

    echo "Zellij installed successfully."
}

# Check if Zellij is installed
if ! command -v zellij &> /dev/null
then
    echo "Checking for Zellij compatibility..."

    # Check system architecture
    ARCH=$(uname -m)
    case "$ARCH" in
        x86_64)
            install_zellij "x86_64-unknown-linux-musl"
            ;;
        aarch64)
            install_zellij "aarch64-unknown-linux-musl"
            ;;
        *)
            # Unsupported architecture, do nothing but print a message
            echo "Unsupported architecture: $ARCH. Zellij installation is skipped."
            ;;
    esac
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

# Start Zellij or tmux in SSH connections
if [[ $SSH_CONNECTION ]]; then
  if [[ $- =~ i ]] && [[ -n "$SSH_TTY" ]]; then
    # Check if Zellij is installed and use it
    if command -v zellij &> /dev/null; then
      # Try to attach to the ssh_zellij session, else create one
      if [[ -z "$ZELLIJ" ]]; then
        zellij attach --name ssh_zellij || zellij new --name ssh_zellij
        exit  # Exit afterward
      fi
    # Fallback to tmux if Zellij is not available
    elif command -v tmux &> /dev/null; then
      # Try to attach to the ssh_tmux session, else create one
      if [[ -z "$TMUX" ]]; then
        tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
        exit  # Exit afterward
      fi
    else
      echo "Neither Zellij nor tmux is available."
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
    if command -v fish &> /dev/null; then
        exec fish
    else
      echo "Fish shell not found. Using zsh instead ..."
      exec zsh
    fi
  else
    exec fish
  fi
  exit  # Exit afterward
fi


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
. "$HOME/.cargo/env"
