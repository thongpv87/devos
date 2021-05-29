#!/usr/bin/env bash
echo $(amixer get Master | grep 'Front Left: Playback' | awk -F '\\[|%' '{print $2}')
