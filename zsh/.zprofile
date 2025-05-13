source ~/dotfiles/scripts/hw-info

[ "$HOST" = "arch-desktop" ] && export ULTRAWIDE=1

if gpu_has_amd; then
  export VDPAU_DRIVER=radeonsi
elif gpu_has_intel; then
  export WLR_RENDERER=vulkan
  export VDPAU_DRIVER=va_gl
fi

# Wayland
if [ -z "$WAYLAND_DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  export XDG_CURRENT_DESKTOP=sway:wlroots
  dbus-run-session sway
fi

# X11
# if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
#   export MOZ_USE_XINPUT2=1
#   export MOZ_X11_EGL=1
#   exec startx
# fi
