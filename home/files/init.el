(require 'use-package)

					; Misc

(column-number-mode)

(setq backup-directory-alist
      '(("." . "~/.local/share/emacs/saves/")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)

					; Appearance

(setq-default truncate-lines t)
(setq-default fill-column 80)

;; (use-package all-the-icons)

					; Markup

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/org/school.org"
                           "~/org/projects.org"
                           "~/org/personal.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "INPROG" "|" "DONE" "KILL" "FAIL"))))

(use-package org-journal
  :config
  (setq org-journal-dir "~/org/diary")
  :bind
  ("C-c j" . org-journal-new-entry))

					; Programming

(use-package magit
  :bind
  ("\C-x g" . magit-status))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; (use-package company-posframe
;;   :ensure t
;;   :config
;;   (setq company-posframe-quickhelp-delay nil)
;;   (company-posframe-mode 1))

					; Utility

(use-package ivy
  :config
  (ivy-mode 1))

(use-package dired
  :config
  (setq dired-listing-switches "--group-directories-first -al")
  (setq dired-auto-revert-buffer t
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package dashboard
  :config
  (setq dashboard-items
        '((recents . 5)
          (projects . 5)
          (agenda . 10)))
  (setq dashboard-set-footer nil)
  (dashboard-setup-startup-hook))
