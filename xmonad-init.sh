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

# load xfce settings
xfsettingsd &

# startup trayer

try_run_once /usr/bin/trayer \
	--edge top \
	--align right \
	--SetDockType true\
	--SetPartialStrut true \
	--expand true \
	--width 10 \
	--transparent true \
	--tint 0x191970 \
	--height 12

try_run_once /usr/bin/nm-applet --sm-disable

try_run_once /usr/bin/xfce4-power-manager

try_run_once /usr/bin/xfce4-volumed

# IM
export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"

try_run_once /usr/bin/ibus-daemon -dxr
