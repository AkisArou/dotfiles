#!/bin/sh

shutdown="Shutdown"
reboot="Reboot"
suspend="Suspend"
hibernate="Hibernate"
logout="Logout"

case $(printf "%s\n" $shutdown $reboot $suspend $hibernate $logout | tofi $@) in
$shutdown)
  systemctl poweroff
  ;;
$reboot)
  systemctl reboot
  ;;
$suspend)
  systemctl suspend
  ;;
$hibernate)
  systemctl hibernate
  ;;
$logout)
  pkill -KILL -u "$USER"
  ;;
esac
