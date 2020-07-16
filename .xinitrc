#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

userresources=$HOME/.Xresources

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

setxkbmap -layout us,ir
setxkbmap -option "grp:shifts_toggle" -option "ctrl:nocaps"
systemctl --user import-environment DISPLAY
exec i3
