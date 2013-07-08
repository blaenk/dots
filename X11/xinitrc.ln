if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

eval $(gnome-keyring-daemon)
export GNOME-KEYRING-SOCKET
export GNOME-KEYRING-PID

export $(gnome-keyring-daemon --start)

xsetroot -cursor_name left_ptr
numlockx &
nitrogen --restore
xkbcomp -I$HOME/.xkb ~/.xkb/keymap/blaenk $DISPLAY
xrdb -merge ~/.Xresources # remove -merge if problematic
xset r rate 250 50
# trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x2f2f2f --expand true --heighttype pixel --height 20

compton -cGb
# compton --config ~/.compton.conf -b
exec xmonad

