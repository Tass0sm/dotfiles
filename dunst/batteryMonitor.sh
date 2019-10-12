#!/bin/bash

while :
do
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "percentage" | read foo
    echo "$foo"
done
