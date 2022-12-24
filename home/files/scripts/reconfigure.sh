#!/usr/bin/env sh

LOCAL_CHANNEL="$HOME/software/tassos-guix"
HOME_ENV_DIR="$HOME/software/dotfiles/"
ENV_FILE="$HOME_ENV_DIR/home/environment.scm"

# need to use this to prepend to the load path.
guix home reconfigure -L $HOME_ENV_DIR -L $LOCAL_CHANNEL $@ $ENV_FILE
