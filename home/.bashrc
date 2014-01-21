#
# ~/.bashrc
#

Color_Off='\e[0m'       # Text Reset
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# PRE-X
export PATH=$PATH:/home/tzbob/bin:/opt/android-sdk/platform-tools:/home/tzbob/.cabal/bin

# If not running interactively, don't continue
[[ $- != *i* ]] && return
PS1="\[$Purple\]\w\[$Color_Off\] "

export HISTSIZE=50000
shopt -s histappend

export EDITOR="vim"
export BROWSER="chromium"
export WINEARCH="win32"

# fix QTGTK
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

#--------------------------------------------------
# aliases
#-------------------------------------------------- 

alias subs="subliminal --languages en nl -q "
alias o="mimeopen"
alias syu="sudo aura -Syu && sudo aura -Akua"
alias df="df -h"
alias ls="ls -F --color=auto"
alias grep="grep --color=auto"

alias sus="systemctl suspend"
alias hib="systemctl hibernate"
alias off="systemctl poweroff"

alias sudo="sudo "

alias b="cd ~/bin"
alias v="cd /media/data/Videos"
alias m="cd /media/data/Music"
alias t="cd ~/Dropbox/thesis/pdfs/"
alias d="cd ~/Downloads"

# vi mode in bash
set -o vi

# enable history completion
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# add to cd path
CDPATH=".:~/Dropbox"

# sudo auto complete
complete -cf sudo

#--------------------------------------------------
# functions
#--------------------------------------------------

vid() {
  subliminal --languages en nl -q $@ 2>&1 /dev/null
  if [ -f "$@" ]
  then
    mimeopen "$@"
  else
    mimeopen *$@*
  fi
}

extr () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1        ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       rar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1        ;;
            *.tbz2)      tar xjf $1      ;;
            *.tgz)       tar xzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via extr()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

timesleep() {
  TIME=$(date --date "+6 hours" +%s)
  if [ $# -gt 0 ]
  then
    TIME=$(date --date "$@" +%s)
  fi
  sudo rtcwake -m mem -t $TIME
  mplayer "/media/data/Music/IRIS OST [MP3]/05 Jooni - Empty.mp3"
}

pu() {
  dropbox puburl "$@" | xsel
}

cd() {
  builtin cd "$@" && ls
}

copy() {
  cp -r "$@" ~/.drag
}

cut() {
  mv "$@" ~/.drag
}

paste() {
  mv ~/.drag/* ./
}

launch() {
  nohup "$@" > /dev/null 2>&1&
}

search() {
  aura -Ss "$1" && aura -As "$1"
}
ls

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"
