##################################
#  EXPORTS begin                 #
##################################

# Check if nvim is available
if command -v nvim &>/dev/null; then
  # Use nvim for manpages
  # export MANPAGER="nvim -c 'set ft=man' -"
  # Emacsclient as (sudo-)editor
  export EDITOR="nvim"
  export SUDO_EDITOR="nvim"
  alias vim=nvim
else
  # Emacsclient as (sudo-)editor
  export EDITOR="vim"
  export SUDO_EDITOR="vim"
fi

# FZF options
export FZF_DEFAULT_OPTS='--height 40% --border'
export FZF_DEFAULT_COMMAND='ag -g .'


# Fixes some terminal application colors
export TERM="xterm-256color"


##################################
#  EXPORTS end                   #
##################################


# Start tmux in ssh connections
if [[ $SSH_CONNECTION ]]; then
  if [ "$TMUX" = "" ]; then
    tmux
  fi
fi




##################################
#  INSTALL BINARIES begin        #
##################################

# Ensure pip is installed
if ! command -v pip &> /dev/null; then
  curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
  python3 /tmp/get-pip.py
fi

# Check if direnv is installed
if [ ! -f $HOME/bin/direnv ]; then
  echo "Direnv not found. Installing now ..."
  mkdir -p $HOME/bin
  wget -O $HOME/bin/direnv https://github.com/direnv/direnv/releases/download/v2.20.0/direnv.linux-amd64 > /dev/null
  chmod +x $HOME/bin/direnv
fi

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

  zgen load esc/conda-zsh-completion

  zgen load zsh-users/zsh-autosuggestions
  # zgen load zsh-users/zsh-syntax-highlighting
  zgen load zdharma-continuum/fast-syntax-highlighting

  # Load Prezto
  zgen prezto

  # Load Prezto Modules
  # zgen prezto git
  # zgen prezto environment
  # zgen prezto terminal
  # zgen prezto editor
  zgen prezto history
  # zgen prezto directory
  # zgen prezto spectrum
  # zgen prezto utility
  zgen prezto completion  # Must be loaded after utility
  # zgen prezto syntax-highlighting
  # zgen prezto history-substring-search
  # zgen prezto autosuggestions
  # zgen prezto prompt

  zgen prezto utility safe-ops 'no'

  # generate the init script from plugins above
  zgen save
fi


ZSH_AUTOSUGGEST_STRATEGY=(history completion)

##################################
#  ZGEN end                      #
##################################



##################################
#  ZSH INTERNAL SETTINGS begin   #
##################################

# vi mode
bindkey -v
export KEYTIMEOUT=1

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^F" forward-char


##################################
#  ZSH INTERNAL SETTINGS end   #
##################################



##################################
#  MISC begin                    #
##################################

# Enable fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/.conda/etc/profile.d/conda.sh ] && source ~/.conda/etc/profile.d/conda.sh

# Enable direnv
eval "$(direnv hook zsh)"


source ~/.bash_aliases


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
# Add maybe_activate_conda_env as chpwd (change working directory) hook
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd maybe_activate_conda_env

autopair-init

# Function to fix Wacom Graphic tablet to HDMI-0 output
function fix-wacom () {
  xinput map-to-output $(xinput | grep stylus | awk -F= '{print $2}' | awk -F\[ '{ print $1 }') HDMI-0
}

##################################
#  MISC end                      #
##################################
