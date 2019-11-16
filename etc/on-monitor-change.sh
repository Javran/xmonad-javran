#!/bin/bash

if [ "$HOSTNAME" = Sajuuk ]; then
    if xrandr | grep "VGA1 connected"; then
        xrandr --output VGA1 --right-of eDP1 --auto
    else
        xrandr --output VGA1 --off
    fi
elif [ "$HOSTNAME" = Senatus ]; then
    xrandr --output HDMI-0 --primary
    xrandr --output DP-1 --right-of HDMI-0 --auto
fi

numlockx
