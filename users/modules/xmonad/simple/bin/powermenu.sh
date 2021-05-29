#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x

# Available Styles
# >> Created and tested on : rofi 1.6.0-1
#
# column_circle     column_square     column_rounded     column_alt
# card_circle     card_square     card_rounded     card_alt
# dock_circle     dock_square     dock_rounded     dock_alt
# drop_circle     drop_square     drop_rounded     drop_alt
# full_circle     full_square     full_rounded     full_alt
# row_circle      row_square      row_rounded      row_alt
res_width=$1

theme="card_circle"
dir="$HOME/.config/rofi/powermenu"

# random colors
#styles=($(ls -p --hide="colors.rasi" $dir/styles))
#color="${styles[$(( $RANDOM % 8 ))]}"

# comment this line to disable random colors
#sed -i -e "s/@import .*/@import \"$color\"/g" $dir/styles/colors.rasi

# comment these lines to disable random style
#themes=($(ls -p --hide="powermenu.sh" --hide="styles" --hide="confirm.rasi" --hide="message.rasi" $dir))
#theme="${themes[$(( $RANDOM % 24 ))]}"

uptime=$(uptime -p | sed -e 's/up //g')
rofi_command="rofi -theme $dir/${theme}_${res_width}"

# Options
shutdown="襤"
reboot="ﰇ"
lock=""
suspend=""
logout="﫼"

# Confirmation
confirm_exit() {
  action=$1
  rofi -dmenu\
    -i\
    -no-fixed-num-lines\
    -p "Do you want to ${action}? : "\
    -theme $dir/confirm.rasi
}

perform_action() {
    ans=$1
    action=$2
    if [[ $ans == "yes" || $ans == "YES" || $ans == "y" || $ans == "Y" ]]; then
        eval $action
    elif [[ $ans == "no" || $ans == "NO" || $ans == "n" || $ans == "N" ]]; then
        exit 0
    else
        rofi -theme "$dir/message.rasi" -e "Available Options  -  yes / y / no / n"
    fi
}

# Variable passed to rofi
options="$shutdown\n$reboot\n$lock\n$suspend\n$logout"

chosen="$(echo -e "$options" | $rofi_command -p "Uptime: $uptime" -dmenu -selected-row 2)"
case $chosen in
    $shutdown)
    ans=$(confirm_exit "shutdown" &)
    perform_action "$ans" "shutdown -h now"
    ;;
    $reboot)
        ans=$(confirm_exit "reboot" &)
        perform_action "$ans" "shutdown -h now"
    ;;
    $lock)
        betterlockscreen -l
    ;;
    $suspend)
        ans=$(confirm_exit "suspend" &)
        perform_action "$ans" "playerctl pause & amixer set Master mute & systemctl suspend"
    ;;
    $logout)
        ans=$(confirm_exit "logout"&)
        perform_action "$ans" "killall xmonad-x86_64-linux"
    ;;
esac
