#!/bin/bash

background="#2f2f2f"
foreground="#c5c8c6"

YPOS="2"
HEIGHT="16"

FONT="DejaVu Sans Mono:pixelsize=14:antialias=true:hinting=true"

XPOS=810
WIDTH="195"
LINES="8"

day="s/\($(date +%d)"
calendar=$(cal | tail -n +2 | sed 's/^\(.*\)$/  \1  /g' | sed "$day"'\)/\^fg\(\#b32d47\)\1\^fg\(\)/g')

(echo ""; echo ""; echo "$calendar") | dzen2 -p -fg $foreground -bg $background -fn "$FONT" -x $XPOS -y $YPOS -w $WIDTH -l $LINES -e 'onstart=uncollapse,hide;button1=exit;button3=exit;leaveslave=exit' -xs 1

