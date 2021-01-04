#!/bin/sh

# Load jack sink and source modules,
pactl load-module module-jack-sink channels=2
pactl load-module module-jack-source channels=2

# and redirect audio to them.
pacmd set-default-sink jack_out
pacmd set-default-source jack_in
