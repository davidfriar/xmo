#!/bin/bash

# Simple script to handle a DIY shutdown menu. When run you should see a bunch of options (shutdown, reboot etc.)
#
# Requirements:
# - rofi
# - systemd, but you can replace the commands for OpenRC or anything else
#
# Instructions:
# - Save this file as power.sh or anything
# - Give it exec priviledge, or chmod +x /path/to/power.sh
# - Run it

chosen=$(echo -e "1 Cancel\n2 Lock\n3 Shutdown\n4 Reboot\n5 Suspend\n6 Hibernate" | rofi -dmenu -i)
# Info about some states are available here:
# https://www.freedesktop.org/software/systemd/man/systemd-sleep.conf.html#Description

if [[ $chosen = "2 Lock" ]]; then
	i3lock-fancy
elif [[ $chosen = "3 Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = "4 Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "5 Suspend" ]]; then
	systemctl suspend
elif [[ $chosen = "6 Hibernate" ]]; then
	systemctl hibernate
fi
