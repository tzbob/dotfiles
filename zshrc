#-*-sh-*-

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
bindkey -M vicmd 'k' history-beginning-search-backward
bindkey -M vicmd 'j' history-beginning-search-forward

source ~/.localAlias

alias em="emacsclient -c -a emacs"

alias o="xdg-open"
alias df="df -h"
alias ls="ls -F --color=auto"

alias sus="systemctl suspend"
alias hib="systemctl hibernate"
alias off="systemctl poweroff"
alias reb="systemctl reboot"

alias sudo="sudo "

alias b2w="spd-say 'back to work'"

alias b='let a="60*15" && sleep $a && b2w'

alias pidofsbtfork="ps aux | grep '[F]orkMain' | awk {'print \$2'}"
alias pidofsbtrun="ps aux | grep '[c]om.cleverbase.core.platform.Main' | awk {'print \$2'}"

alias structurizr-lite="docker run -it --rm -p 9004:8080 -v $PWD:/usr/local/structurizr structurizr/lite"
alias appium="docker run --privileged -d -p 4723:4723  -v /dev/bus/usb:/dev/bus/usb -e ADB_SHELL=true --name container-appium appium/appium"

alias clever-tool="scala-cli ~/clever-tools/scala-cli-tools --main-class"

function chpwd() {
    emulate -L zsh
    ls -a
}

function pdf2ps2pdf() {
    temp=/tmp/"$1".ps
    pdf2ps "$1" $temp
    ps2pdf $temp min."$1"
}

runcmd (){ perl -e 'ioctl STDOUT, 0x5412, $_ for split //, <>' ; }
fh() {
    ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -re 's/^\s*[0-9]+\s*//' | runcmd
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if [[ $1 == eval ]]
then
	"$@"
	set --
fi

# >>> scala-cli completions >>>
fpath=("/home/bob/.local/share/scalacli/completions/zsh" $fpath)
compinit
# <<< scala-cli completions <<<
