#!/bin/sh

case $(printf "%s\n" "Shut down" "Reboot" "Suspend" "Hibernate" | tofi $@) in
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
esac
