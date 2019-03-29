#!/bin/sh

# if [ -d /etc/X11/xinit/xinitrc.d ]; then
#         for f in /etc/X11/xinit/xinitrc.d/*; do
#                 [ -x "$f" ] && . "$f"
#         done
#         unset f
# fi

# eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# export GPG_AGENT_INFO SSH_AUTH_SOCK

# export QT_QPA_PLATFORMTHEME=qt5ct
# export GDK_SCALE=1
# export QT_AUTO_SCREEN_SCALE_FACTOR=1

# export GTK_IM_MODULE=ibus
# export QT4_IM_MODULE=xim
# export QT_IM_MODULE=xim
# export XMODIFIERS=@im=ibus

# xset +dpms
# xset dpms 0 0 300

# xrdb -merge ~/.Xresources
# xsetroot -cursor_name left_ptr

# ~/bin/mods &
# ~/bin/toggleIkbd.sh &

# syndaemon -i .5 -KtRd &

# nitrogen --restore &

# nm-applet &
# redshift -l 51.2:4.4 &

# xcape -e 'Alt_L=BackSpace;Alt_R=Return' &

# date >> ~/.xmonad/log

# trayer --align center --widthtype request --height 40 --tint 0x333333 --transparent true --alpha 0 --monitor "primary" &

# ~/.xmonad/xmonad-x86_64-linux >> ~/.xmonad/log 2>&1
