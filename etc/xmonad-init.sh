#!/bin/bash

# args: (exec-location . exec-args)
function try_run_once()
{
	EXEC=$1
	shift

	if [ -x "$EXEC" ] && ! pgrep -f "$EXEC" >/dev/null ; then
		echo run: $EXEC
		"$EXEC" $* &
	else
		echo running or not exist: $EXEC
	fi
}

# load Xresources
xrdb -merge ~/.Xresources

if (lsusb | grep "HHKB"); then
    echo "HHKB keyboard detected."
    cp ~/.xmonad/XmodmapHHKB ~/.Xmodmap
else
    echo "HHKB keyboard not detected."
    cp ~/.xmonad/XmodmapDef ~/.Xmodmap
fi

xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr

# $WALLPAPER could be set up somewhere in .xinitrc for example
xloadimage -onroot -center -type png "$WALLPAPER_PATH" -geometry "$WALLPAPER_GEOMERTY"

~/.xmonad/on-monitor-change.sh

pulseaudio --start

[ ! -s ~/.config/mpd/pid ] && mpd

try_run_once /usr/bin/nm-applet --sm-disable

try_run_once /usr/bin/xfce4-power-manager

# try_run_once /usr/bin/xfce4-volumed

try_run_once /usr/bin/xscreensaver -no-splash

# IM
try_run_once /usr/bin/fcitx -dr

# try_run_once /usr/bin/pidgin

pkill SysInfoBar; ~/.xmonad/SysInfoBar &

#try_run_once ~/.dropbox-dist/dropboxd

# startup trayer
pkill -9 trayer-srg

try_run_once /usr/bin/trayer-srg \
	--edge top \
	--align right \
	--SetDockType true \
	--SetPartialStrut true \
	--expand true \
	--width 11 \
	--transparent true \
	--tint 0xe0e0e0 \
	--height 24

# load xfce settings
xfsettingsd &

wmname LG3D

# seems not working,
# guess xfsettingsd had changed something
# maybe it's time to drop xfce entirely
# numlockx
