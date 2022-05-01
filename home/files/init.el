;;; -*- lexical-binding: t -*-

                                        ; Misc

(column-number-mode)

(setq backup-directory-alist
      '(("." . "~/.local/share/emacs/saves/")))


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq inhibit-startup-screen t)

;; (setq pop-up-frames t)

(use-package recentf
  :config
  (recentf-mode 1))

                                        ; Server

(defun server-new-frame-buffer ()
  "Get the buffer to be used when making a new frame for the emacs
server."
  (window-buffer (selected-window)))

(defun server-new-terminal-file ()
  "Get the file to be used when making a new terminal while using
the emacs server."
  (let ((buffer (server-new-frame-buffer)))
    (expand-file-name (cdr (assq 'default-directory
                                 (buffer-local-variables buffer))))))

                                        ; Basic Tools

(use-package ivy
  :config
  (ivy-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (require 'subr-x)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package dired
  :config
  (setq dired-listing-switches "-lahv --group-directories-first"
        dired-auto-revert-buffer t
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package cape
  :init
  ;; This modifies the default value of completion-at-point-functions. This will
  ;; be tried after the capfs in the buffer local value.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (setq cape-dabbrev-check-other-buffers nil))

(use-package corfu
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-quit-at-boundary t
        corfu-auto-delay 0
        corfu-cycle t
        corfu-preselect-first nil)
  (unbind-key "RET" corfu-map)
  (corfu-global-mode)
  :hook (shell-mode . (lambda ()
                        (setq-local corfu-quit-at-boundary t
                                    corfu-quit-no-match t
                                    corfu-auto nil))))

(use-package helpful
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)))

(use-package popper
  :bind
  (("C-`"   . popper-toggle-latest)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Backtrace\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "shell\\*$"
          "^magit"
          org-agenda-mode
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir))
  :config
  ;; (setq consult-dir-shadow-filenames nil)
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))

(use-package direnv
  :config
  (direnv-mode 1))

(use-package avy
  :bind
  (("M-j" . avy-goto-word-1)))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package unkillable-scratch
  :config
  (unkillable-scratch t))

                                        ; Tool Modes

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package guix-popup
  :bind
  ("C-x y" . guix-popup))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package pdf-tools
  :magic ("%PDF" .  pdf-view-mode)
  :hook (pdf-view-mode . pdf-isearch-minor-mode))

(use-package notmuch
  :config
  (setq-default notmuch-search-oldest-first nil))

(use-package message
  :config
  (setq message-send-mail-function 'mailclient-send-it))

                                        ; Specific Editing Modes

(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c C-e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c C-e" . macrostep-expand))))

(use-package geiser-guile
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-guile-load-path '("~/software/guix/")))


(use-package web-mode
  :mode ("\\.html" . web-mode))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2))

(use-package add-node-modules-path
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :after (js2-mode add-node-modules-path)
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package python
  :config
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
  )

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

                                        ; General Editing Modes

(setq-default indent-tabs-mode nil)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package phi-search
  :bind
  ((:map mc/keymap
         ("C-s" . phi-search)
         ("C-r" . phi-search-backward))))

(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              (("C-,"   . flyspell-auto-correct-word)
               ("C-."   . flyspell-goto-next-error)
               ("C-;"   . flyspell-correct-next)
               ("C-M-;" . flyspell-buffer)
               ("C-M-i" . nil))))

                                        ; Markup

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-agenda-list))
  :config
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/org/school.org"
                           "~/org/projects.org"
                           "~/org/personal.org"
                           "~/org/work.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "INPROG" "|" "DONE" "KILL" "FAIL")))
  (setq org-edit-src-content-indentation 0))

(use-package ob
  :after (org jupyter)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (dot . t)
     (python . t)
     ;;(jupyter . t)
     )))

(use-package ox-latex
  :config
  (setq org-latex-pdf-process (list "latexmk -f -pdf %f")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (setq org-roam-directory (file-truename
                            (concat org-directory "/notes/")))
  (org-roam-db-autosync-mode))

(use-package org-journal
  :bind
  ("C-c j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/org/diary"))

(use-package org-download
  :config
  (setq org-download-display-inline-images nil
        org-download-screenshot-method "flameshot gui --raw > %s"))

(use-package org-notify
  :config
  (org-notify-add 'default '(:time "1h" :actions -notify/window
                                   :period "10m" :duration 30))
  (org-notify-add 'class   '(:time "30m" :actions -notify/window
                                   :period "10m" :duration 30))
  (org-notify-start))

(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package markdown-mode)

                                        ; Appearance

(setq-default truncate-lines t)
(setq-default fill-column 80)

(if (daemonp)
    (cl-labels ((load-nord (frame)
                           (with-selected-frame frame
                             (load-theme 'nord t))
                           (remove-hook 'after-make-frame-functions #'load-nord)))
      (add-hook 'after-make-frame-functions #'load-nord))
  (load-theme 'nord t))
