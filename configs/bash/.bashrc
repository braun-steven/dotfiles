#
# ~/.bashrc
#
# Go into zsh shell

# Add ruby binaries to path if available
if command -v ruby &> /dev/null; then
  PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
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


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# # Eval keychain only locally
# # NOTE: This needs to be done after the interactive if-statement
# if [[ -z $SSH_CONNECTION ]]; then
#   eval $(keychain --eval --quiet id_rsa)
# fi

# Go into zsh
if [[ $(ps --no-header --pid=$PPID --format=cmd) != "zsh" ]]; then
  exec zsh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
