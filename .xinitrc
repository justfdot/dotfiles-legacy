#!/bin/sh

userresources=$HOME/.Xresources
# usermodmap=$HOME/.Xmodmap
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

# if [ -f "$usermodmap" ]; then
#   xmodmap "$usermodmap"
# fi

# # start some nice programs
# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#   for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#     # shellcheck source=/dev/null
#     [ -x "$f" ] && . "$f"
#   done
#   unset f
# fi

# exec заменяет запущенный в данный момент командный процессор (тот, что выполняет этот скрипт xinitrc) на этот процесс. Когда пользователь его закроет, сеанс X будет завершён.

xset s noblank
xset s 0 0
xset s off
xset -dpms
xsetroot -solid black

urxvtd --quiet --opendisplay --fork
syndaemon -t -d -K
xbindkeys
compton -b
redshift &
eyeskeeper &

exec i3
# exec herbstluftwm --locked
