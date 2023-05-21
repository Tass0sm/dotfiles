#!/usr/bin/env bash

# bspc desktop -f 'next.local' &&

PWD=$(pwd)
DATE=$(date --rfc-3339=seconds | sed 's/ /-/')

ffplay -f v4l2 -video_size 640x480 -i /dev/video0 &
ffmpeg -f x11grab -s 1920x1080 -i :0.0  \
       -f pulse -i default \
       $PWD/take-$DATE.mp4
