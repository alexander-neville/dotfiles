#!/bin/bash

rofi_command="rofi -theme $HOME/.config/rofi/themes/screenshot.rasi"

# Options
screen="Whole Desktop"
area="Selected Area"
clipboard="Clipboard Copy"
window="Focused Window"

# Variable passed to rofi
options="$screen\n$area\n$clipboard\n$window"

chosen="$(echo -e "$options" | $rofi_command -p 'Screenshot Tool' -dmenu -selected-row 1)"
case $chosen in
    $screen)
        sleep 0.4; scrot 'img_%d-%m-%Y_%H:%M:%S_$wx$h.png' -e 'mv $f $$(xdg-user-dir PICTURES)/screenshots ; viewnior $$(xdg-user-dir PICTURES)/screenshots/$f'
        ;;
    $clipboard)
        scrot -se 'xclip -selection clipboard -t image/png -i $f ; rm -f $f'
        ;;
    $area)
        scrot -s 'img_%d-%m-%Y_%H:%M:%S_$wx$h.png' -e 'mv $f $$(xdg-user-dir PICTURES)/screenclips ; viewnior $$(xdg-user-dir PICTURES)/screenclips/$f'
        ;;
    $window)
        sleep 0.4; scrot -u 'img_%d-%m-%Y_%H:%M:%S_$wx$h.png' -e 'mv $f $$(xdg-user-dir PICTURES)/screenclips ; viewnior $$(xdg-user-dir PICTURES)/screenclips/$f'
        ;;
esac

# time_stamp=`date +"%H:%M:%S_%d-%m-%y"`
# file=~/image/screenshots/IMG_$time_stamp.png
