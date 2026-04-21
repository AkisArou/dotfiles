source ~/dotfiles/scripts/hw-info

export DEFAULT_THEME=vscode

[ "$HOST" = "arch-desktop" ] && export ULTRAWIDE=1

if gpu_has_amd; then
  export VDPAU_DRIVER=radeonsi
elif gpu_has_intel; then
  export WLR_RENDERER=vulkan
  export VDPAU_DRIVER=va_gl
fi

# Base functions
config_wayland() {
  export XDG_SESSION_TYPE=wayland
  export SDL_VIDEODRIVER=wayland
  export _JAVA_AWT_WM_NONREPARENTING=1
  # export QT_QPA_PLATFORM=wayland # Disable for android emulator for now
}

config_xorg() {
  export MOZ_USE_XINPUT2=1
  export MOZ_X11_EGL=1
}

# WM functions
run_sway() {
  config_wayland
  export XDG_CURRENT_DESKTOP=sway
  export XDG_SESSION_DESKTOP=sway
  exec dbus-run-session sway
}

run_hypr() {
  config_wayland
  start-hyprland
}

run_mango() {
  config_wayland
  exec dbus-run-session mango
}

run_i3() {
  config_xorg
  exec startx
}

run_dwl() {
  config_wayland
  export XDG_CURRENT_DESKTOP=wlroots
  export XDG_SESSION_DESKTOP=wlroots
  exec dbus-run-session dwl
  # >/home/akisarou/.cache/dwltags
}

# Only run session chooser on first virtual terminal (e.g., tty1)
if [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ] && [ -z "$WAYLAND_DISPLAY" ] && [ -z "$DISPLAY" ]; then
  echo "Select session type:"
  echo "1) dwl (default)"
  echo "2) Sway"
  echo "3) hyprland"
  echo "4) i3"
  printf "Enter choice [1-3]: "
  read session_choice

  session_choice=${session_choice:-1}

  if [ "$session_choice" = "1" ]; then
    run_dwl
  elif [ "$session_choice" = "2" ]; then
    run_sway
  elif [ "$session_choice" = "3" ]; then
    run_hypr
  elif [ "$session_choice" = "4" ]; then
    run_i3
  else
    echo "Falling back to default (dwl)."
    run_sway
  fi
fi
