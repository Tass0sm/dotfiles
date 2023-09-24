;;; -*- lexical-binding: t -*-

                                        ; Misc

(column-number-mode)

(setq backup-directory-alist
      '(("." . "~/.local/share/emacs/saves/")))


(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (load custom-file)
  (write-region "" nil custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq scroll-conservatively 5)

(use-package recentf
  :init
  (recentf-mode 1))

                                        ; Server

(defun tm/desired-buffer ()
  "Get the buffer to be used when making a new frame for the emacs
server."
  (window-buffer (selected-window)))

(defun tm/desired-directory (&optional root)
  "Get the file to be used when making a new terminal while using
the emacs server."
  (let ((d (expand-file-name
            (cdr (assq 'default-directory
                       (buffer-local-variables (tm/desired-buffer)))))))
    (if root
        (project-root (project-current nil d))
      d)))

(defun tm/new-term (&optional root)
  "Open a new terminal in the desired directory"
  (interactive)
  (let* ((default-directory (tm/desired-directory root))
         (vterm-buffer-name (format
                             "*%s-vterm*"
                             (if (project-current nil)
                                 (f-base (project-root (project-current nil)))
                               (f-base default-directory)))))
    (vterm)))

                                        ; Basic Tools

(use-package vertico
  :bind (:map minibuffer-local-map
              ("C-j" . vertico-insert)
              ("M-DEL" . vertico-directory-delete-word))
  :init
  (vertico-mode 1))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package orderless
  :init
  (setq completion-category-defaults nil
        completion-styles '(orderless basic)
        ;; For tramp:
        completion-category-overrides '((file (styles basic partial-completion)))))

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
  :custom
  ;; Circumvents an error when using frames-only-mode
  (embark-verbose-indicator-display-action
   '(display-buffer-in-side-window (side . right))))

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult))
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
         ;; Isearch integration
;;          ("M-s e" . consult-isearch-history)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;          ;; Minibuffer history
;;          :map minibuffer-local-map
;;          ("M-s" . consult-history)                 ;; orig. next-matching-history-element

         ;; orig. previous-matching-history-element
         ;; ("M-r" . consult-history)
         )

  ;; ;; The :init configuration is always executed (Not lazy)
  ;; :init

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
  )

(use-package which-key
  :init
  (which-key-mode 1))

(use-package project
  :config
  (defun project-vterm ()
    "Open a new terminal in the desired directory"
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (format
                               "*%s-vterm*"
                               (if (project-current nil)
                                   (f-base (project-root (project-current nil)))
                                 (f-base default-directory)))))
      (vterm)))

  (bind-key "v" 'project-vterm 'project-prefix-map)
  (bind-key "r" 'consult-ripgrep 'project-prefix-map)
  (bind-key "m" 'magit-project-status 'project-prefix-map)

  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-vterm "VTerm")
          (consult-ripgrep "Ripgrep")
          (magit-project-status "Magit"))))

(use-package project-x
  :after project
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90))

(use-package dired
  :config
  (setq dired-listing-switches "-lahv --group-directories-first"
        dired-auto-revert-buffer t
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package cape
  :config
  ;; In increasing precedence.
  (setq cape-dict-file (expand-file-name "~/.guix-home/profile/share/web2"))
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-dict)
  (add-to-list 'completion-at-point-functions 'cape-file)
  (setq cape-dabbrev-check-other-buffers nil))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :bind (:map corfu-map
              ;; I want it to be easy to get out of corfu
              ([remap move-beginning-of-line] . nil)
              ([remap next-line] . nil)
              ([remap previous-line] . nil)
              ("RET" . nil)
              ;; A single, non-conflicting way to complete
              ("C-j" . corfu-complete))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-quit-at-boundary t
        corfu-auto-delay 0.0
        corfu-cycle t
        corfu-preselect-first t)
  :hook (shell-mode . (lambda ()
                        (setq-local corfu-quit-at-boundary t
                                    corfu-quit-no-match t
                                    corfu-auto nil))))

(use-package popper
  :init
  (popper-mode 1)
  (popper-echo-mode 1)
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
          help-mode
          helpful-mode
          compilation-mode))
  (setq popper-window-height
        (lambda (win)
          (fit-window-to-buffer
           win
           (floor (frame-height) 2)))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir))
  :config
  ;; (setq consult-dir-shadow-filenames nil)
  (setq consult-dir-project-list-function #'consult-dir-project-dirs))

(use-package envrc
  :config
  (envrc-global-mode 1))

(use-package avy
  :bind
  (("M-j" . avy-goto-word-1)))

(use-package unkillable-scratch
  :config
  (unkillable-scratch t))

(use-package bufler
  :init
  (bufler-mode 1)
  :bind ("C-x C-b" . bufler)
  :config
  (setf bufler-groups (bufler-defgroups
                        (group
                         ;; Subgroup collecting all named workspaces.
                         (auto-workspace))
                        (group
                         ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                         (group-or "*Help/Info*"
                                   (mode-match "*Help*" (rx bos "help-"))
                                   (mode-match "*Info*" (rx bos "info-"))))
                        (group
                         ;; Subgroup collecting all special buffers (i.e. ones that are not
                         ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
                         ;; through to other groups, so they end up grouped with their project buffers).
                         (group-and "*Special*"
                                    (lambda (buffer)
                                      (unless (or (funcall (mode-match "Vterm" (rx bos "vterm"))
                                                           buffer)
                                                  (funcall (mode-match "Magit" (rx bos "magit-status"))
                                                           buffer)
                                                  (funcall (mode-match "Dired" (rx bos "dired"))
                                                           buffer)
                                                  (funcall (auto-file) buffer))
                                        "*Special*")))
                         (group
                          ;; Subgroup collecting these "special special" buffers
                          ;; separately for convenience.
                          (name-match "**Special**"
                                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
                         (group
                          ;; Subgroup collecting all other Magit buffers, grouped by directory.
                          (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
                          (auto-directory))
                         ;; Subgroup for Helm buffers.
                         (mode-match "*Helm*" (rx bos "helm-"))
                         ;; Remaining special buffers are grouped automatically by mode.
                         (auto-mode))
                        ;; All buffers under "~/.emacs.d" (or wherever it is).
                        (dir user-emacs-directory)
                        (group
                         ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
                         ;; `org-directory' is not yet defined).
                         (dir (if (bound-and-true-p org-directory)
                                  org-directory
                                "~/org"))
                         (group
                          ;; Subgroup collecting indirect Org buffers, grouping them by file.
                          ;; This is very useful when used with `org-tree-to-indirect-buffer'.
                          (auto-indirect)
                          (auto-file))
                         ;; Group remaining buffers by whether they're file backed, then by mode.
                         (group-not "*special*" (auto-file))
                         (auto-mode))
                        (group
                         ;; Subgroup collecting buffers in a version-control project,
                         ;; grouping them by directory.
                         (auto-project))
                        ;; Group remaining buffers by directory, then major mode.
                        (auto-directory)
                        (auto-mode))))

(use-package bufler-workspace
  :init
  (bufler-workspace-mode 1)

  (defun set-workspace ()
    "Set workspace based on current directory."
    (interactive)
    (let ((path (bufler-buffer-workspace-path (current-buffer))))
      (bufler-workspace-frame-set path)))

  (defun clear-workspace ()
    "Set workspace based on current directory."
    (interactive)
    (bufler-workspace-frame-set nil))

  :bind (("C-x w s" . set-workspace)
         ("C-x w c" . clear-workspace)))

(use-package jupyter)

(use-package frames-only-mode
  :init
  (frames-only-mode 1))

(use-package citar
  :config
  (add-to-list 'citar-file-open-functions
               '("pdf" . citar-file-open-external))

  (setq citar-bibliography '("~/documents/research/bibliography.bib")
        citar-library-paths '("~/documents/research/library/")
        citar-notes-paths '("~/org/notes/citar/")))

(use-package citar-latex)

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config
  (setq citar-org-roam-subdir "citar")
  (citar-org-roam-mode 1))


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

(use-package vterm
  :config
  (setq vterm-timer-delay 0.01))

(use-package pdf-tools
  :magic ("%PDF" .  pdf-view-mode)
  :hook ((pdf-view-mode . pdf-isearch-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)))

(use-package tex
  :config
  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Sioyek")
          (output-html "xdg-open"))))

(use-package notmuch
  :config
  (setq-default notmuch-search-oldest-first nil))

(use-package message
  :config
  (setq message-send-mail-function 'mailclient-send-it))


(use-package eglot)

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
  :hook (python-mode . eglot-ensure)
  :config
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
  )

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

(use-package cmake-mode)

                                        ; General Editing Modes

(setq-default indent-tabs-mode nil)

(use-package treesit
  :config
  (setq treesit-extra-load-path '("~/.guix-home/profile/lib/tree-sitter/")))

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

(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-expand)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("<tab>" . tempel-next)
         ("<backtab>" . tempel-previous))
  :config
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-complete nil t))

  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf))


(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              (("C-,"   . flyspell-auto-correct-word)
               ("C-."   . flyspell-goto-next-error)
               ("C-;"   . flyspell-correct-next)
               ("C-M-;" . flyspell-buffer)
               ("C-M-i" . nil))))

                                        ; Markup

(use-package org-ql)

(use-package org
  :bind (("C-c a"   . org-agenda)
         ("C-c o a"   . org-agenda)
         ("C-c o l"   . org-store-link)
         ("C-c o c"   . org-capture)
         ("C-c o f"   . consult-org-agenda))
  :config
  (setq org-log-done 'time)
  (setq org-default-notes-file (f-join org-directory "notes.org"))
  (setq org-startup-indented t)
  (setq org-todo-keywords
        '((sequence "TODO" "INPROG" "|" "DONE" "KILL" "FAIL")))
  (setq org-edit-src-content-indentation 0))

(use-package org-capture
  :config
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 1)))))

(use-package org-agenda
  :config
  (setq org-agenda-files (list "~/org/school.org"
                               "~/org/projects.org"
                               "~/org/personal.org"
                               "~/org/work.org"
                               (f-join org-directory "notes.org")
                               "~/org/notes/projects"))
  (setq org-agenda-custom-commands
        '(("g" "Goals" ((todo "" ((org-agenda-tag-filter-preset '("+goal"))))))
          ("ces" "Custom: QL Todos"
           ((org-ql-block '(todo)))))))



(use-package oc
  :after citar
  :bind (:map org-mode-map ("C-c b" . #'org-cite-insert))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-global-bibliography citar-bibliography))

(use-package oc-biblatex
  :config
  (setq org-cite-biblatex-styles `(,@org-cite-biblatex-styles
                                   ("parencite" nil "parencite" "parencites")
                                   ("footcite" nil "footcite" "footcites")
                                   ("footfullcite" nil "footfullcite" "footfullcites")
                                   ("supercite" nil "supercite" "supercites"))))

(require 'org-analyzer)

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
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-pdf-process (list "latexmk -f -pdf %f")))

(use-package ox-haunt)

(use-package org-roam
  :demand
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
  (require 'ucs-normalize)
  (setq org-roam-directory (file-truename
                            (concat org-directory "/notes/")))
  (org-roam-db-autosync-mode))

(use-package org-roam-dailies
  :bind
  ("C-c n d" . org-roam-dailies-goto-today)
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))))

(use-package org-download
  :config
  (setq org-download-display-inline-images nil
        org-download-screenshot-method "flameshot gui --raw > %s"))

(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package markdown-mode)

                                        ; Appearance

(setq-default truncate-lines t)
(setq-default fill-column 80)

(defun load-nord (&optional frame)
  "Load nord theme for emacs."
  (if frame
      (progn
        (with-selected-frame frame
          (load-theme 'nord t))
        (remove-hook 'after-make-frame-functions #'load-nord))
    (load-theme 'nord t))
  (let ((line          (face-attribute 'mode-line          :background))
        (line-inactive (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'mode-line          nil :box line)
    (set-face-attribute 'mode-line-inactive nil :box line-inactive)))

(use-package nord-theme
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-nord)
    (load-nord)))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  ;; Iosevka needs to be built with all of these, but it isn't by default.
  (ligature-set-ligatures
   'prog-mode
   '("-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__"
     "<~~" "</" "</>" "/>" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
     "<:" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+:" "-:" "=:" ":>"
     "(*" "*)" "/*" "*/" "[|" "|]" "{|" "|}" "++" "+++" "|-" "-|" "<!--" "<!---"))
  (global-ligature-mode t))
