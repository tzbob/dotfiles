#-*-sh-*-
typeset -U path
path=(/opt/android-sdk/platform-tools ~/bin ~/.cabal/bin ~/.local/bin $path)

cdpath=(~ ~/Dropbox)

export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=vim EDITOR="emacsclient -c -a emacs" VISUAL="emacsclient -c -a emacs"
export BROWSER='chromium'
export JAVA_HOME=/usr/lib/jvm/default
export ANDROID_HOME=/opt/android-sdk
export QT_STYLE_OVERRIDE="GTK+"

export WINEARCH=win32
