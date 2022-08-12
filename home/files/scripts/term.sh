#!/usr/bin/env sh

if [ ! $(pidof emacs) ]; then
    alacritty
elif [ $# -ne 0 ]; then
    emacsclient -c -n -e "(tm/new-term $@)"
else
    emacsclient -c -n -e "(tm/new-term)"
fi
