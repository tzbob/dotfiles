#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
        for f in /etc/X11/xinit/xinitrc.d/*; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO SSH_AUTH_SOCK

xset +dpms
xset dpms 0 0 300

xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr

~/bin/mods &
~/bin/toggleIkbd.sh &

syndaemon -i .5 -KtRd &

nitrogen --restore &

compton &
trayer --align center --widthtype request --height 32 --SetDockType true --alpha 0 --transparent true --tint 0x00fafafa &

nm-applet &
redshift -l 51.2:4.4 &

date >> ~/.xmonad/log
~/.xmonad/xmonad-x86_64-linux >> ~/.xmonad/log 2>&1
