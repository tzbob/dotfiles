#-*-sh-*-
typeset -U path
path=(/opt/android-sdk/platform-tools ~/bin ~/.cabal/bin ~/.local/bin ~/.local/share/coursier/bin $path)

cdpath=(~ ~/Dropbox)

export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=vim EDITOR="emacsclient -c -a emacs" VISUAL="emacsclient -c -a emacs"
export BROWSER='firefox'
export ANDROID_HOME=~/Android/SDK
export SBT_OPTS="-XX:ReservedCodeCacheSize=512m -Xmx4096m -XX:+UseG1GC -XX:SoftRefLRUPolicyMSPerMB=50 -XX:CICompilerCount=4"
export _JAVA_AWT_WM_NONREPARENTING=1
