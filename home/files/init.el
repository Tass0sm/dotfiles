(require 'use-package)

					; Misc hello world

(column-number-mode)

(setq backup-directory-alist
      '(("." . "~/.local/share/emacs/saves/")))


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)

(setq inhibit-startup-screen t)

;; (setq pop-up-frames t)

					; Server

(defun server-new-frame-buffer ()
  "Get the buffer to be used when making a new frame for the emacs
server."
  (let* ((oldframe (selected-frame))
	 (oldwindow (frame-selected-window oldframe))
	 (oldbuffer (window-buffer oldwindow)))
    (cond
     ((null oldbuffer) (get-buffer-create "*scratch*"))
     (t oldbuffer))))

(defun server-new-terminal-file ()
  "Get the file to be used when making a new terminal while using
the emacs server."
  (let* ((oldframe (selected-frame))
	 (oldwindow (frame-selected-window oldframe))
	 (oldbuffer (window-buffer oldwindow))
	 (dir (cdr (assq 'default-directory
			 (buffer-local-variables oldbuffer)))))
    (cond
     ((stringp dir) (expand-file-name dir))
     (t "/home/tassos"))))

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
  (global-company-mode 1))

;; (use-package company-posframe
;;   :hook (company-mode . company-posframe-mode)
;;   :config
;;   (setq company-posframe-quickhelp-delay nil))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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
  :magic ("%PDF" .  pdf-view-mode))

					; Specific Editing Modes

;;(use-package dante
;;  :after haskell-mode
;;  :commands 'dante-mode
;;  :init
;;  (add-hook 'haskell-mode-hook 'flymake-mode)
;;  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;  (add-hook 'haskell-mode-hook 'dante-mode))

(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
	       ("C-c C-e" . macrostep-expand))
	 (:map lisp-interaction-mode-map
	       ("C-c C-e" . macrostep-expand))))

(use-package web-mode
  :mode ("\\.html" . web-mode)
  :config
  (defun set-company-backends-for-web ()
    (setq-local company-backends '(company-capf
				   company-files
				   (company-dabbrev-code company-keywords)
				   company-dabbrev)))
  (add-hook 'web-mode-hook 'set-company-backends-for-web))

					; General Editing Modes

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

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
  (setq org-download-screenshot-method "flameshot gui --raw > %s"))

					; Appearance

(setq-default truncate-lines t)
(setq-default fill-column 80)

;; (use-package all-the-icons)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(select-frame frame)
		(load-theme 'nord)))
  (load-theme 'nord))
