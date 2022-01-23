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
  :config
  (require 'subr-x)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package dired
  :config
  (setq dired-listing-switches "--group-directories-first -al")
  (setq dired-auto-revert-buffer t
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (push 'company-yasnippet company-backends)
  (global-company-mode 1))

;; (use-package company-posframe
;;   :hook (company-mode . company-posframe-mode)
;;   :config
;;   (setq company-posframe-quickhelp-delay nil))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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
  (direnv-mode))

                                        ; Tool Modes

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package guix-popup
  :bind
  ("C-x y" . guix-popup))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package pdf-tools
  :magic ("%PDF" .  pdf-view-mode)
  :hook (pdf-view-mode . pdf-isearch-minor-mode))

                                        ; Specific Editing Modes

;; Lisp
(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c C-e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c C-e" . macrostep-expand))))

;; HTML + jS + CSS
(use-package web-mode
  :mode ("\\.html" . web-mode)
  :config
  (defun set-company-backends-for-web ()
    (setq-local company-backends '(company-yasnippet
                                   company-capf
                                   company-files
                                   (company-dabbrev-code company-keywords)
                                   company-dabbrev)))
  (add-hook 'web-mode-hook 'set-company-backends-for-web))

;; JavaScript

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2)
  (defun set-company-backends-for-js ()
    (setq-local company-backends '(company-yasnippet
                                   company-capf
                                   company-files
                                   (company-dabbrev-code company-keywords)
                                   company-dabbrev)))
  (add-hook 'js2-mode-hook 'set-company-backends-for-js))

(use-package add-node-modules-path
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :after (js2-mode add-node-modules-path)
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

;; Python
(use-package python
  :config
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

  (defun set-company-backends-for-python ()
    (setq-local company-backends '(company-yasnippet
                                   company-capf
                                   company-files
                                   (company-dabbrev-code company-keywords)
                                   company-dabbrev)))
  (add-hook 'python-mode-hook 'set-company-backends-for-python))

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
               ("C-M-;" . flyspell-buffer))))

                                        ; Markup

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/org/school.org"
                           "~/org/projects.org"
                           "~/org/personal.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "INPROG" "|" "DONE" "KILL" "FAIL")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-agenda-list))

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
  :config
  (setq org-journal-dir "~/org/diary")
  :bind
  ("C-c j" . org-journal-new-entry))

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
