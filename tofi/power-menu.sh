#!/bin/sh

case $(printf "%s\n" "Shut down" "Reboot" "Suspend" "Hibernate" "Logout" | tofi $@) in
"Shut down")
  systemctl poweroff
  ;;
"Reboot")
  systemctl reboot
  ;;
"Suspend")
  systemctl suspend
  ;;
"Hibernate")
  systemctl hibernate
  ;;
"Logout")
  swaymsg exit && exit
  ;;
esac
