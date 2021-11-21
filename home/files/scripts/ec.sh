#!/usr/bin/env sh

if [ $# -eq 0 ]; then
    emacsclient -c -n -a "" -e "(switch-to-buffer (server-new-frame-buffer))"
else
    emacsclient -c -n -a "" $@
fi
