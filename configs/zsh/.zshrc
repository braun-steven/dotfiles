source ~/.bash_exports

# Start tmux in ssh connections
if [[ $SSH_CONNECTION ]]; then

# Try to attach to the ssh_tmux session, else create one
  if [[ $- =~ i ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_TTY" ]]; then
    tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
  fi
fi


##################################
#  INSTALL BINARIES begin        #
##################################

# Install neovim
if ! command -v nvim &> /dev/null; then
  curl -L https://github.com/neovim/neovim/releases/latest/download/nvim.appimage --output ~/bin/nvim
  chmod u+x ~/bin/nvim
fi

# Ensure pip is installed
# if ! command -v pip &> /dev/null; then
#   curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
#   python3 /tmp/get-pip.py
# fi

# Check if tpm is installed
if [ ! -d $HOME/.tmux/plugins/tpm ]; then
  echo "Tmux plugin manager not found. Installing now ..."
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Check if eza is installed
if ! command -v eza &> /dev/null; then
  curl -L https://github.com/eza-community/eza/releases/latest/download/eza_x86_64-unknown-linux-gnu.zip --output /tmp/eza.zip
  unzip /tmp/eza.zip -d ~/.local/bin
  chmod u+x ~/.local/bin/eza
fi


# Download and install fzf
if [[ ! -d $HOME/.fzf ]]; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

##################################
#  INSTALL BINARIES end          #
##################################

##################################
#  ZGEN begin                    #
##################################

if [[ ! -d "${HOME}/.zgen" ]]; then
  git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi

# load zgen
source "${HOME}/.zgen/zgen.zsh"

AUTOPAIR_INHIBIT_INIT=1

# if the init script doesn't exist
if ! zgen saved; then

  # specify plugins here
  # prezto options
  zgen prezto editor key-bindings 'emacs'
  zgen prezto prompt theme 'pure'

  # Load general plugins
  zgen load hlissner/zsh-autopair
  zgen load agkozak/zsh-z
  zgen load mafredri/zsh-async
  zgen load junegunn/fzf shell
  zgen load ptavares/zsh-direnv

  zgen load esc/conda-zsh-completion

  zgen load zsh-users/zsh-autosuggestions
  # zgen load zsh-users/zsh-syntax-highlighting
  zgen load zdharma-continuum/fast-syntax-highlighting

  # Load Prezto
  zgen prezto

  # Load Prezto Modules
  # zgen prezto git
  zgen prezto environment
  zgen prezto terminal
  # zgen prezto editor
  zgen prezto history
  # zgen prezto directory
  # zgen prezto spectrum
  zgen prezto utility
  zgen prezto completion  # Must be loaded after utility
  # zgen prezto syntax-highlighting
  # zgen prezto history-substring-search
  # zgen prezto autosuggestions
  # zgen prezto prompt

  zgen prezto utility safe-ops 'no'

  # generate the init script from plugins above
  zgen save
fi

##################################
#  ZGEN end                      #
##################################



##################################
#  ZSH INTERNAL SETTINGS begin   #
##################################

ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# vi mode
bindkey -v
export KEYTIMEOUT=1

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^F" forward-char
bindkey "^E" end-of-line
bindkey "^A" beginning-of-line

setopt SHARE_HISTORY

##################################
#  ZSH INTERNAL SETTINGS end   #
##################################

##################################
#  MISC begin                    #
##################################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $HOME/.bash_functions

# Add maybe_activate_conda_env as chpwd (change working directory) hook
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd maybe_activate_conda_env

autopair-init

autoload edit-command-line; zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Source aliases finally
source ~/.bash_aliases

# Hook direnv
eval "$(direnv hook zsh)"

export TERM=xterm-256color

ulimit -n 30000  # Allow up to 30k open files at the same time

##################################
#  MISC end                      #
##################################


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/steven/.conda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
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

# argc-completions
export ARGC_COMPLETIONS_ROOT="/home/steven/argc-completions"
export ARGC_COMPLETIONS_PATH="$ARGC_COMPLETIONS_ROOT/completions"
export PATH="$ARGC_COMPLETIONS_ROOT/bin:$PATH"
# To add completions for only the specified command, modify next line e.g. argc_scripts=( cargo git )
argc_scripts=( $(ls -p -1 "$ARGC_COMPLETIONS_ROOT/completions" | sed -n 's/\.sh$//p') )
source <(argc --argc-completions zsh $argc_scripts)
