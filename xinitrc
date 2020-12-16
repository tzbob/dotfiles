#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO SSH_AUTH_SOCK

export QT_QPA_PLATFORMTHEME=qt5ct
export GDK_SCALE=1
export QT_AUTO_SCREEN_SCALE_FACTOR=1

ibus-daemon -drx
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

xset +dpms
xset dpms 0 0 300

xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr

nitrogen --restore &

nm-applet &
redshift-gtk &

xcape -e 'Alt_L=BackSpace;Alt_R=Return' &

date >> ~/.xmonad/log

.screenlayout/gwangju.sh

~/bin/mods

dropbox-cli start &

exec ~/.xmonad/xmonad-x86_64-linux >> ~/.xmonad/log 2>&1
