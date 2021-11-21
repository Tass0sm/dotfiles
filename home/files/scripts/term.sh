#!/usr/bin/env sh

if [ $# -ne 0 ]; then
    alacritty $@
elif [ ! $(pidof emacs) ]; then
    alacritty
else
    BUF=$(emacsclient -e "(server-new-terminal-file)" | tr -d '"')
    alacritty --working-directory $BUF
fi
