#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
  xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
  xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
  xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    # shellcheck source=/dev/null
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#twm &
#xclock -geometry 50x50-1+1 &
#xterm -geometry 80x50+494+51 &
#xterm -geometry 80x20+494-0 &
#exec xterm -geometry 80x66+0+0 -name login

# exec заменяет запущенный в данный момент командный процессор (тот, что выполняет этот скрипт xinitrc) на этот процесс. Когда пользователь его закроет, сеанс X будет завершён.

# exec i3


feh --bg-scale ~/img/wallpaper
setxkbmap "us,ru" ",winkeys" "ctrl:nocaps,grp_led:caps"
urxvtd --quiet --opendisplay --fork
syndaemon -t -d -K
compton &
redshift &
eyeskeeper &
exec herbstluftwm --locked
