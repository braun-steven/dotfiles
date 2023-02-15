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

# Add maybe_activate_conda_env as chpwd (change working directory) hook
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd maybe_activate_conda_env

autopair-init

autoload edit-command-line; zle -N edit-command-line
bindkey "^X^E" edit-command-line


# Hook direnv
eval "$(direnv hook zsh)"

##################################
#  MISC end                      #
##################################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

