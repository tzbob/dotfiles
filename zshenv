#-*-sh-*-
typeset -U path
path=(/opt/android-sdk/platform-tools ~/bin ~/.cabal/bin ~/.local/bin $path)

cdpath=(~ ~/Dropbox)

export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=vim EDITOR="emacsclient -c -a emacs" VISUAL="emacsclient -c -a emacs"
export BROWSER='chromium'
export JAVA_HOME=/usr/lib/jvm/default
export ANDROID_HOME=~/Android/SDK
export _JAVA_AWT_WM_NONREPARENTING=1
