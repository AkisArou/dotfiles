source ~/dotfiles/scripts/hw-info

[ "$HOST" = "arch-desktop" ] && export ULTRAWIDE=1

if gpu_has_amd; then
  export VDPAU_DRIVER=radeonsi
elif gpu_has_intel; then
  export WLR_RENDERER=vulkan
  export VDPAU_DRIVER=va_gl
fi

# Only run session chooser on first virtual terminal (e.g., tty1)
if [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ] && [ -z "$WAYLAND_DISPLAY" ] && [ -z "$DISPLAY" ]; then
  echo "Select session type:"
  echo "1) Wayland (default)"
  echo "2) Xorg"
  printf "Enter choice [1-2]: "
  read session_choice

  session_choice=${session_choice:-1}

  if [ "$session_choice" = "1" ]; then
    export XDG_CURRENT_DESKTOP=sway:wlroots
    exec dbus-run-session sway
  elif [ "$session_choice" = "2" ]; then
    export MOZ_USE_XINPUT2=1
    export MOZ_X11_EGL=1
    exec startx
  else
    echo "Invalid choice. Falling back to default (Wayland)."
    export XDG_CURRENT_DESKTOP=sway:wlroots
    exec dbus-run-session sway
  fi
fi
