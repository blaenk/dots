#!/usr/bin/env bash

PIPE=/tmp/volume-pipe

fifocheck() {
  if [[ ! -p $PIPE ]]; then
    if [[ -f $PIPE ]]; then
      rm $PIPE
    fi

    mkfifo $PIPE
  fi
}

volume() {
  # with %:
  # amixer get Master | awk -F '[][]' '/%/{print $2; exit}'
  # without %:
  amixer get Master | awk -F '[]%[]' '/%/{print $2; exit}'
}

transmit() {
  STATE=`amixer get Master | awk -F '[][]' '/%/{print $4; exit}'`

  if [ x$STATE == x"on" ]; then
    COLORED="<fc=#718c00><icon=/home/jorge/.xmonad/icons/volume.xbm/></fc>  "`volume`
    echo $COLORED > $PIPE
  else
    COLORED="<fc=#b32d47><icon=/home/jorge/.xmonad/icons/muted.xbm/></fc>  muted"
    echo $COLORED > $PIPE
  fi

  mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga
}

up() {
  fifocheck
  amixer -q set Master,0 unmute
  amixer -q set Master,0 2%+
  transmit
}

down() {
  fifocheck
  amixer -q set Master,0 unmute
  amixer -q set Master,0 2%-
  transmit
}

toggle() {
  fifocheck
  amixer -q set Master,0 toggle
  transmit
}

establish() {
  fifocheck
  transmit
}

case "$1" in
  up )
    up;;
  down )
    down;;
  toggle )
    toggle;;
  * )
    establish;;
  esac

