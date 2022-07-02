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
  :init
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

(use-package vertico
  :init
  (vertico-mode 1))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  ;; completion-category-defaults nil
  ;; completion-category-overrides '((file (styles partial-completion))))
  )

(use-package marginalia
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-align 'right
        marginalia-align-offset -5)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult))
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings (mode-specific-map)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c k" . consult-kmacro)
;;          ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;          ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g I" . consult-imenu-multi)
;;          ;; M-s bindings (search-map)
;;          ("M-s d" . consult-find)
;;          ("M-s D" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s L" . consult-line-multi)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
;;          ;; Isearch integration
;;          ("M-s e" . consult-isearch-history)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;          ;; Minibuffer history
;;          :map minibuffer-local-map
;;          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;          ("M-r" . consult-history))                ;; orig. previous-matching-history-element

;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init

;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key (kbd "M-."))
;;   ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-bookmark consult--source-recent-file
;;    consult--source-project-recent-file
;;    :preview-key (kbd "M-."))

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; By default `consult-project-function' uses `project-root' from project.el.
;;   ;; Optionally configure a different project root function.
;;   ;; There are multiple reasonable alternatives to chose from.
;;   ;;;; 1. project.el (the default)
;;   ;; (setq consult-project-function #'consult--default-project--function)
;;   ;;;; 2. projectile.el (projectile-project-root)
;;   ;; (autoload 'projectile-project-root "projectile")
;;   ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;   ;;;; 3. vc.el (vc-root-dir)
;;   ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;   ;;;; 4. locate-dominating-file
;;   ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;; )

(use-package which-key
  :init
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
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-quit-at-boundary t
        corfu-auto-delay 0
        corfu-cycle t
        corfu-preselect-first nil)
  (unbind-key "RET" corfu-map)
  :hook (shell-mode . (lambda ()
                        (setq-local corfu-quit-at-boundary t
                                    corfu-quit-no-match t
                                    corfu-auto nil))))

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

(use-package unkillable-scratch
  :config
  (unkillable-scratch t))


(use-package jupyter)

(use-package frames-only-mode
  :config
  (frames-only-mode 1))

                                        ; Tool Modes

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package guix-popup
  :init
  (global-guix-prettify-mode 1)
  :hook
  (scheme-mode . guix-devel-mode)
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
  (setq geiser-active-implementations '(guile)))

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
     (jupyter . t))))

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
