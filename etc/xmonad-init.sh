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

xloadimage -onroot -center -type png "${WALLPAPER}"

# startup trayer
pkill -9 trayer

try_run_once /usr/bin/trayer \
	--edge top \
	--align right \
	--SetDockType true \
	--SetPartialStrut true \
	--expand true \
	--width 11 \
	--transparent true \
	--tint 0xe0e0e0 \
	--height 24

try_run_once /usr/bin/nm-applet --sm-disable

try_run_once /usr/bin/xfce4-power-manager

try_run_once /usr/bin/xfce4-volumed

try_run_once /usr/bin/xscreensaver -no-splash

# IM
try_run_once /usr/bin/fcitx -dr

try_run_once /usr/bin/pidgin

try_run_once ~/.xmonad/MailChecker

#try_run_once ~/.dropbox-dist/dropboxd

# load xfce settings
xfsettingsd &

wmname LG3D

# seems not working,
# guess xfsettingsd had changed something
# maybe it's time to drop xfce entirely
numlockx
