source ~/dotfiles/scripts/hw-info

[ "$HOST" = "arch-desktop" ] && export ULTRAWIDE=1

if gpu_has_amd; then
  export VDPAU_DRIVER=radeonsi
elif gpu_has_intel; then
  export WLR_RENDERER=vulkan
  export VDPAU_DRIVER=va_gl
fi

wayland() {
  export SDL_VIDEODRIVER=wayland
  export _JAVA_AWT_WM_NONREPARENTING=1
  export QT_QPA_PLATFORM=wayland
  export XDG_CURRENT_DESKTOP=sway
  export XDG_SESSION_DESKTOP=sway
  exec dbus-run-session sway
}

xorg() {
  export MOZ_USE_XINPUT2=1
  export MOZ_X11_EGL=1
  exec startx
}

# Only run session chooser on first virtual terminal (e.g., tty1)
if [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ] && [ -z "$WAYLAND_DISPLAY" ] && [ -z "$DISPLAY" ]; then
  echo "Select session type:"
  echo "1) Wayland (default)"
  echo "1) Xorg"
  printf "Enter choice [1-2]: "
  read session_choice

  session_choice=${session_choice:-1}

  if [ "$session_choice" = "1" ]; then
    wayland
  elif [ "$session_choice" = "2" ]; then
    xorg
  else
    echo "Falling back to default (Wayland)."
    wayland
  fi
fi
