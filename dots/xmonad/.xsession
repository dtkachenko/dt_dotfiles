#!/bin/bash

# Load resources

xrdb -merge .Xresources

# Set up an icon tray

stalonetray &

#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
# --expand true --width 10 --transparent true --tint 0x191970 --height 12 &
# Set the background color<

#xsetroot -solid midnightblue
xsetroot -solid gray

# Fire up apps

gnome-settings-daemon &

#xscreensaver -no-splash &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 3
   gnome-power-manager &
fi

exec xmonad
