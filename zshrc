# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

setopt HIST_IGNORE_SPACE

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/
ZSH_THEME="tzbob"

COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git sbt systemd vi-mode extract)

source $ZSH/oh-my-zsh.sh

# User configuration
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

export PATH=$PATH:/home/tzbob/bin:/opt/android-sdk/platform-tools:/home/tzbob/.cabal/bin:/home/bob/.local/bin
cdpath=(~ ~/Dropbox)

export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=vim EDITOR=vim VISUAL=gvim
export BROWSER='chromium'

source ~/.localAlias

alias subs="subberthehut -qsf "
alias o="mimeopen"
alias syu="sudo aura -Syu && sudo aura -Akua"
alias df="df -h"
alias ls="ls -F --color=auto"
alias emt='emacsclient -a "" -nw'
alias emn='emacsclient -a "" -c -n'

alias sus="systemctl suspend"
alias hib="systemctl hibernate"
alias off="systemctl poweroff"
alias reb="systemctl reboot"

alias sudo="sudo "

alias b="cd ~/bin"
alias t="cd ~/Torrents"
alias d="cd ~/Downloads"

source "$HOME/.homesick/repos/homeshick/homeshick.sh"

function chpwd() {
    emulate -L zsh
    ls -a
}

function search() {
  pacman -Ss "$1" && aura -As "$1"
}

function mktex() {
  find "$@" | entr latexmk "$@" -pdf
}

function pdf2ps2pdf() {
    temp=/tmp/"$1".ps
    pdf2ps "$1" $temp
    ps2pdf $temp min."$1"
}
