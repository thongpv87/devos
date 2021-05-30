#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x

# >> Created and tested on : rofi 1.6.0-1

theme="style"

dir="$HOME/.config/rofi/launcher"

rofi -no-lazy-grab -matching fuzzy -show combi \
-combi-modi run,drun \
-theme $dir/"$theme" -drun-icon-theme "candy-icons"
