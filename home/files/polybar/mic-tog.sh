#!/bin/sh

if [[ $(pamixer --default-source --get-mute) == "true" ]]; then
  echo "" # Muted Icon
else
  echo "" # Unmuted Icon
fi
