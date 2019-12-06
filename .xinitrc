#!/bin/sh

# /etc/X11/xinit/xinitrc
#
# global xinitrc file, used by all X sessions started by xinit (startx)

# invoke global X session script
#. /etc/X11/Xsession
xrdb ~/.Xresources
xrandr | grep "^eDP-1" > /dev/null 2>&1
internal_monitor_connected=$?

xrandr | grep "^HDMI-1" > /dev/null 2>&1
hdmi_monitor_connected=$?

setxkbmap -layout us,ir
setxkbmap -option "grp:shift_caps_toggle"

#[ $internal_monitor_connected -eq 0 ] && [ $hdmi_monitor_connected -eq 0 ] && ~/.config/i3/2_monitor.sh && exec i3
exec i3

