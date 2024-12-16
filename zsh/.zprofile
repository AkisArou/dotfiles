if [[ "$HOST" == "arch-desktop" ]]; then
  export VDPAU_DRIVER=va_gl
else
  # export VDPAU_DRIVER=va_gl
fi

# Wayland
if [ -z "$WAYLAND_DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  XDG_CURRENT_DESKTOP=sway:wlroots WLR_RENDERER=vulkan dbus-run-session sway
fi

# X11
# if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
#   exec startx
