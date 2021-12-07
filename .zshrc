# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

export PATH=$HOME/.local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/$USER/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git history-substring-search direnv zsh-syntax-highlighting zsh-autosuggestions autojump)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

alias dvorak-keyboard="gsettings set org.gnome.desktop.input-sources sources \"[('xkb', 'us+dvorak'), ('xkb', 'us'), ('xkb', 'gb')]\""
alias us-keyboard="gsettings set org.gnome.desktop.input-sources sources \"[('xkb', 'us'), ('xkb', 'us+dvorak'), ('xkb', 'gb')]\""

alias start-vpn="openvpn3-autoload --directory ~/Documents/openvpn3/autoload"
alias stop-vpn="~/Documents/openvpn3/stop-vpn.sh"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

alias enable-swap-escape="gsettings set org.gnome.desktop.input-sources xkb-options \"['caps:swapescape']\""
alias disable-swap-escape="gsettings set org.gnome.desktop.input-sources xkb-options \"['']\""
alias mt="make test"
alias mtv="py.test -vvv"
alias mf="make fixup"
alias ml="make lint"
alias mfl="make fixup; make lint"
alias mc="make commit"
alias e="emacs &"
alias et="emacs -nw"
alias ed="emacs --daemon=emacs-server"
alias edf="emacsclient -s emacs-server --create-frame &"
alias edt="emacsclient -nw -s emacs-server"
alias ds="~/.emacs.d/bin/doom sync"
alias du="~/.emacs.d/bin/doom --yes upgrade"
alias dr="django-admin runserver"
alias c="code ."
alias cl="config pull"
alias ca="config add"
alias capa="config add --patch"
alias cc="config commit -v"
alias cdd="config diff"
alias cds="config diff --staged"
alias cpp="config push"
alias cst="config status"
alias cstall="config stash --all"
alias csta="config stash apply"
alias nrs="npm run start"
alias dcu="docker-compose up"
alias ke="killall emacs"

update_os_packages(){
    if cat /etc/os-release | grep -q "arch";
    then
        sudo pacman -Syu --noconfirm && yay -Syu --answerclean None --answerdiff None
    else
        sudo apt-get update && sudo apt-get -y upgrade
    fi
}
alias ul="update_os_packages"
alias u="update_os_packages && ~/.emacs.d/bin/doom --yes upgrade"

setopt share_history
bindkey '\ef' emacs-forward-word

export PYTHONSTARTUP="$HOME/code/github-libraries/pythonrc/pythonrc.py"
is_wsl=$(grep -c WSL /proc/version)
if [ "$PC_USAGE" = "work" ];
then
 export DISPLAY=$(ip route | awk '/default via / {print $3; exit}' 2>/dev/null):0.0
 export LIBGL_ALWAYS_INDIRECT=1


 alias pytest="pytest --disable-warnings" # should only disable specific warnings

 xset r rate 250 100
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH=$HOME/code/utilities:$PATH
