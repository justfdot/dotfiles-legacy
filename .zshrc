# -*- mode: zsh; sh-indentation: 2; indent-tabs-mode: nil; sh-basic-offset: 2; -*-
# vim: ft=zsh sw=2 ts=2 et

# Path to your oh-my-zsh installation.
ZSH=$HOME/.oh-my-zsh/

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="justfdot"

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
COMPLETION_WAITING_DOTS="true"

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
alias tmux="tmux -f $HOME/.config/tmux/config"
plugins=(vi-mode cp sudo autopair git gitfast fast-syntax-highlighting history-search-multi-word history-substring-search)

cdpath=(~/)

# ssh
export SSH_KEY_PATH="$HOME/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

ZSH_DISABLE_COMPFIX="true"
source "$ZSH/oh-my-zsh.sh"
# source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
# BASE16_SHELL=$HOME/.config/base16-shell/
# [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

alias zshrc="$EDITOR $HOME/.zshrc"
alias ya="ping ya.ru"
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias em="emacsclient"
alias rem="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"
alias ag="ag --color-line-number='0;35' --color-match='0;33' --color-path='0;34'"
alias rm="rm -i"
alias to="cd"
alias cal="cal -m"
alias ruman="env LANG=\"ru_RU.UTF-8\" man"
alias ls="ls --color=tty --group-directories-first"
alias clbin="curl -F 'clbin=<-' https://clbin.com"
alias hc="herbstluftclient"


if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="emacsclient"
fi
export VISUAL="$EDITOR"
export BROWSER="/usr/bin/google-chrome-stable"
export XZ_OPT="--threads=6"
export YDCMD_TOKEN="1b8605ea9b0144c9af32e504c73921e6"
export PYTHONPATH="$PYTHONPATH:$HOME/.local/bin/"
# export MANPATH="/usr/local/man:$MANPATH"

ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

export KEYTIMEOUT=20
bindkey -M viins 'df' vi-cmd-mode


# History substring search
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=yellow,fg=black'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=black'
HISTORY_SUBSTRING_SEARCH_FUZZY=true
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=true
bindkey '' history-substring-search-up
bindkey '' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red'
ZSH_HIGHLIGHT_STYLES[function]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=green'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=green'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=green'
ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[command]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=yellow, bold'
ZSH_HIGHLIGHT_STYLES[hashed-commands]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[path]='underline'

typeset -gA HSMW_HIGHLIGHT_STYLES
HSMW_HIGHLIGHT_STYLES[unknown-token]='fg=red'
HSMW_HIGHLIGHT_STYLES[function]='fg=cyan'
HSMW_HIGHLIGHT_STYLES[globbing]='fg=green'
HSMW_HIGHLIGHT_STYLES[single-quoted-argument]='fg=green'
HSMW_HIGHLIGHT_STYLES[double-quoted-argument]='fg=green'
HSMW_HIGHLIGHT_STYLES[alias]='fg=cyan'
HSMW_HIGHLIGHT_STYLES[builtin]='fg=yellow'
HSMW_HIGHLIGHT_STYLES[command]='fg=yellow'
HSMW_HIGHLIGHT_STYLES[precommand]='fg=yellow, bold'
HSMW_HIGHLIGHT_STYLES[hashed-commands]='fg=yellow'
HSMW_HIGHLIGHT_STYLES[path]='underline'

zstyle ":history-search-multi-word" highlight-color "fg=red"
zstyle ":plugin:history-search-multi-word" active "bold"

# export WORKON_HOME=~/.virtualenvs
# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
# source /usr/bin/virtualenvwrapper.sh

# ##############################
# USER FUNCTIONS
# ##############################

back () {
  if [ -f "$1" ]; then
    cp "$1"{,.orig}
  else
    echo "'$1' is not a valid file"
  fi
}

mask () {
  if [ -f "$1" ]; then
    mv "$1"{,.masked}
  else
    echo "'$1' is not a valid file"
  fi
}

mnt () {
  if [[ $EUID != 0 ]] ; then
    echo "Be root, dude!"
    return 1
  fi
  type=$1
  device=$2
  if [[ -z "$type" ]] ; then
    echo "It's necessary to specify the type of device (mountpoint in /mnt)"
    return 1
  fi
  if [[ ! -d "/mnt/$type" ]] ; then
    echo "Unknown type of device: $type"
    return 1
  fi
  [[ -z "$device" ]] && device="/dev/sdb1"
  if [[ ! -b "$device" ]] ; then
    echo "There is no such block device: $device"
    return 1
  fi
  mount -o 'rw,umask=000' "$device" "/mnt/$type"
}

umnt () {
  if [[ $EUID != 0 ]] ; then
    echo "Be root, dude!"
    return 1
  fi
  type=$1
  if [[ -z "$type" ]] ; then
    echo "It's necessary to specify the type of device (mountpoint in /mnt)"
    return 1
  fi
  if [[ ! -d "/mnt/$type" ]] ; then
    echo "Unknown type of device: $type"
    return 1
  fi
  if ! mount | grep -q "/mnt/$type" ; then
    echo "Device is not mount"
    return 1
  fi
  umount "/mnt/$type"
}

fixcue () {
  if [ -f "$1" ]; then
    output=$(echo "$1" | sed -r 's/(.*)(\.cue)/\1\.utf-8\2/')
    iconv -f WINDOWS-1251 -t UTF-8 "$1" -o "$output"
  else
    echo "'$1' is not a valid file"
  fi
}

fixscale () {
  xrandr --output VGA1 --scale 0.99x1
  sleep 12
  xrandr --output VGA1 --scale 1x1
}

showcolors () {
  printf "\t%s\n" \
         "black:   #1d1f21" \
         "red:     #cc6666" \
         "green:   #b5bd68" \
         "yellow:  #f0c674" \
         "blue:    #81a2be" \
         "magenta: #b294bb" \
         "cyan:    #8abeb7" \
         "white:   #c5c8c6"
}

pack () {
  if [ "$1" ]; then
    case "$1" in
      tbz)         tar cjvf $2.tar.bz2 $2 ;;
      tgz)         tar czvf $2.tar.gz $2 ;;
      tar)         tar cpvf $2.tar $2 ;;
      bz2)         bzip $2 ;;
      gz)          gzip -c -9 -n $2 > $2.gz ;;
      zip)         zip -r $2.zip $2 ;;
      7z)          7z a $2.7z $2 ;;
      *)           echo "'$1' cannot be packed via pack()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

unpack () {
  if [ -f "$1" ]; then
    case "$1" in
      *.tar.bz2)   tar xjf $1 ;;
      *.tar.gz)    tar xzf $1 ;;
      *.tar.xz)    tar xzf $1 ;;
      *.bz2)       bunzip2 $1 ;;
      *.rar)       unrar x $1 ;;
      *.gz)        gunzip $1 ;;
      *.tar)       tar xf $1 ;;
      *.tbz2)      tar xjf $1 ;;
      *.tbz)       tar -xjvf $1 ;;
      *.tgz)       tar xzf $1 ;;
      *.zip)       unzip $1 ;;
      *.Z)         uncompress $1 ;;
      *.7z)        7z x $1 ;;
      *)           echo "I don't know how to unpacked '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
