# Path to your oh-my-zsh installation.
  export ZSH=/home/tak/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="gnzh"
#ZSH_THEME="flazz"
ZSH_THEME="materialshell-dark"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git git-flow-avh mvn)

# User configuration

  export PATH="/home/tak/GNUstep/Tools:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


#alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias cat='ccat'

# Better ls
alias ls='ls -lh --color=auto --group-directories-first'
alias grep='grep --color=auto'

# Pacman shortcuts
alias pacu='sudo pacman -Syyu && yaourt -Syyua'
alias pacs='sudo pacman -S'
alias pacrsc='sudo pacman -Rsc'
alias yaourt='yaourt --noconfirm'
# Moved to update functions (see above)
#alias pacu='sudo pacman -Syu && yaourt -Syua'
#alias pacufull='sudo pacman -Syyu && yaourt -Syua'

# zshrc editing
alias eZ='vim ~/.zshrc'
alias rZ='source ~/.zshrc'
alias reboot='sudo systemctl reboot'
alias poweroff='sudo systemctl poweroff'
# Grub update - (currently systemd-boot -> no need for that)
#alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# Fast ssh-key evaluation
alias ssh-eval-key='eval $(ssh-agent -s) && ssh-add ~/.ssh/id_rsa'

# i3-shortcuts
alias i3config='vim ~/.config/i3/config'
alias i3statusconfig='vim ~/.config/i3status/config'

# Fast terminal-directory navigation
alias xclip='xclip -selection c'
alias PWD='echo $(pwd) | xclip && pwd && echo "path copied"'
alias CD='echo "cd $(xclip -o)" && cd $(xclip -o)'
alias :q='exit'

alias img='feh'
alias pdf='zathura'

alias sshaddtak='eval "$(ssh-agent -s)" && ssh-add ~/.ssh/id_rsa_tak3r07'

alias edit-vim='vim ~/dotfiles/vim/.vimrc'
alias edit-zsh='vim ~/dotfiles/zsh/.zshrc'
alias edit-i3='vim ~/dotfiles/i3/config'
alias edit-i3status='vim ~/dotfiles/i3status/config'

alias ga='git add -v'
alias gaa='git add -A -v'
alias gaad='git add -A -v --dry-run'
alias gc='git commit'
alias gcm='git commit -m'

alias qutebrowser='qutebrowser --backend webengine'

alias gnome-screenshot='gnome-screenshot -a'


export JAVA_HOME=/usr/lib/jvm/java-8-jdk
export TERMINAL=gnome-terminal
export EDITOR=vim
export VISUAL=vim

# Maven java server debugging
export MAVEN_OPTS=-agentlib:jdwp=transport=dt_socket,address=8000,server=y,suspend=n
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
PATH="/home/tak/bin:$PATH"
LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/"
#PATH="/home/tak/gcc:$PATH"

eval $(keychain --eval --quiet id_rsa id_rsa_tak3r07)

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}
