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

alias ag="ag --color-line-number='0;35' --color-match='0;33' --color-path='0;34'"
alias bell="echo -e '\a'"
alias cal="cal -m"
alias clbin="curl -F 'clbin=<-' https://clbin.com"
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias em="emacsclient"
alias ls="ls --color=tty --group-directories-first"
alias rm="rm -i"
alias to="cd"
alias ya="ping ya.ru"


if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="emacsclient"
fi
export VISUAL="$EDITOR"
export BROWSER="/usr/bin/google-chrome-stable"
export XZ_OPT="--threads=6"
export YDCMD_TOKEN="1b8605ea9b0144c9af32e504c73921e6"
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
  for file do
    [[ -f "$file" ]] && cp "$file"{,.orig} || echo "'$file' is not a valid file"
  done
}

mask () {
  for file do
    [[ -f "$file" ]] && mv "$file"{,.masked} || echo "'$file' is not a valid file"
  done
}

unmask () {
  for file do
    [[ -f "$file" ]] && mv "$file" "${file%.*}" || echo "'$file' is not a valid file"
  done
}

mnt () {
  [[ $EUID -eq 0 ]] || { echo "Be root, dude!"; return 1; }
  type=$1
  device=$2
  [[ -n "$type" ]] || { echo "Didn't specify the type of device (mountpoint in /mnt)"; return 1; }
  [[ -d "/mnt/$type" ]] || { echo "Unknown type of device: $type"; return 1; }
  [[ -z "$device" ]] && device="/dev/sdb1"
  [[ -b "$device" ]] || { echo "There is no such block device: $device"; return 1; }
  mount -o 'rw,umask=000' "$device" "/mnt/$type"
}

umnt () {
  [[ $EUID -eq 0 ]] || { echo "Be root, dude!"; return 1; }
  type=$1
  [[ -n "$type" ]] || { echo "Didn't specify the type of device (mountpoint in /mnt)"; return 1; }
  [[ -d "/mnt/$type" ]] || { echo "Unknown type of device: $type"; return 1; }
  mount | grep -q "/mnt/$type" || { echo "Device is not mount"; return 1; }
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

null () {
  cat /dev/null > "$1"
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
