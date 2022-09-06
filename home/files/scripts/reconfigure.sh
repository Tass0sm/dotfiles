#!/usr/bin/env sh

LOCAL_CHANNEL="$HOME/software/tassos-guix"
GUIX_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/guix"
ENV_FILE="$GUIX_DIR/home/environment.scm"

# need to use this to prepend to the load path.
GUILE_LOAD_PATH=$GUILE_LOAD_PATH:$GUIX_DIR \
    guix home reconfigure -L $LOCAL_CHANNEL $@ $ENV_FILE
