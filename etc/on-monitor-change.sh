#!/bin/bash

# echo triggered >>/tmp/vga

if xrandr | grep "VGA1 connected"; then
    # echo turning on >>/tmp/vga
    xrandr --output VGA1 --right-of eDP1 --auto
else
    # echo turning off >>/tmp/vga
    xrandr --output VGA1 --off
fi

# echo command sent $? >>/tmp/vga
numlockx
