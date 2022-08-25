#!/usr/bin/env bash

emacsclient -a "" -c -e "(let ((org-agenda-window-setup 'only-window))
                              (org-todo-list))"
