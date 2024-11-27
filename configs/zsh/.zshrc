# ZINIT
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

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
#  ZINIT begin                   #
##################################

# if [[ ! -f ~/.zinit/bin/zinit.zsh ]]; then
#   mkdir -p ~/.zinit/bin && curl -fsSL https://git.io/zinit-install | bash
# fi

# source ~/.zinit/bin/zinit.zsh

# zinit ice wait'0'
# zinit light sorin-ionescu/prezto

# zinit ice wait'0' lucid
zinit light hlissner/zsh-autopair
# zinit light junegunn/fzf
zinit light ptavares/zsh-direnv
# zinit light esc/conda-zsh-completion
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting
zinit snippet PZTM::environment
zinit snippet PZTM::history
zinit snippet PZTM::utility
zinit snippet PZTM::completion

# Load pure theme
zinit ice pick"async.zsh" src"pure.zsh" # with zsh-async library that's bundled with it.
zinit light sindresorhus/pure

autoload -Uz compinit
compinit

##################################
#  ZINIT end                     #
##################################


##################################
#  ZSH INTERNAL SETTINGS begin   #
##################################

# ZSH_AUTOSUGGEST_STRATEGY=(history completion)

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

autoload edit-command-line; zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Source aliases finally
source ~/.bash_aliases

# Hook direnv not necessary due to zsh/direnv plugin, see zinit block | TODO delete
# eval "$(direnv hook zsh)"

export TERM=xterm-256color

ulimit -n 30000  # Allow up to 30k open files at the same time

# If eza binary is available, overwrite ls
if command -v eza &> /dev/null; then
  alias ls='eza -l --group-directories-first --color auto'
fi

##################################
#  MISC end                      #
##################################

eval "$(zoxide init zsh)"

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
