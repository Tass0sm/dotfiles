#!/usr/bin/env sh

if [ $# -eq 0 ]; then
    emacsclient -c -n -a "" -e "(switch-to-buffer (tm/desired-buffer))"
else
    emacsclient -c -n -a "" $@
fi
