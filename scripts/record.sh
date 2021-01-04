#! /bin/sh

ffplay -f video4linux2 -framerate 30 -video_size hd720 /dev/video0 &
ffmpeg -y -f x11grab -s 1920x1080 -i :0.0 -f alsa -i pulse out.mkv
