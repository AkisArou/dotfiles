#!/bin/zsh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

export XDG_SESSION_TYPE="x11"

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

if [ "$HOST" = "arch-xps" ]; then
  xrdb -merge <<<"Xft.dpi: 192"
  export VDPAU_DRIVER=va_gl
else
  export XCURSOR_SIZE=24
fi

exec i3
