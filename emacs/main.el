(use-package avy
  :ensure t)

(use-package tex
  :config
  (add-to-list 'TeX-view-program-list '("mupdf" "/usr/bin/mupdf %o"))  
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf"))
  :chords
  ("34" . TeX-insert-dollar))

(setq backup-directory-alist
      '(("." . "~/.local/share/emacs/saves/")))

(column-number-mode)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-backends '(company-dabbrev))
  (global-company-mode))

;;(use-package company-box
;;  :hook
;;  (company-mode . company-box-mode))

(setq company-active-map '(keymap . ()))
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "C-g") 'company-abort)

(use-package dired
  :config
  (setq dired-listing-switches "--group-directories-first -al")
  (setq dired-auto-revert-buffer t
        dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode))

(setq Info-additional-directory-list '("/home/tassos/Info/"))

(use-package ivy
  :config (ivy-mode 1))

(use-package magit
  :ensure t
  :bind
  ("\C-x g" . magit-status))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package octave
  :config
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(use-package org
  :ensure t
  :config
  (bind-key "C-c a" (lambda () "Open Org-Agenda in New Frame"
                      (interactive)
                      (select-frame (make-frame))
                      (org-agenda-list)
                      (delete-other-windows)))
  (bind-key "C-c t" (lambda () "Open Org-Todo in New Frame"
                      (interactive)
                      (select-frame (make-frame))
                      (org-todo-list)
                      (delete-other-windows)))
  (setq org-agenda-files '("~/Org/school.org"
                           "~/Org/projects.org"
                           "~/Org/personal.org"))
  (setq org-adapt-indentation nil)
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "KILL" "FAIL"))))

(use-package org-notify
  :config
  (org-notify-start)
  (setq org-notify-map nil)
  (org-notify-add 'homework
                  '(:time "6h" :actions -notify/window :duration 60))
  (org-notify-add 'important
                  '(:time "20m" :actions -notify/window :period "2m" :duration 60)))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Org/diary")
  :bind
  ("C-c j" . org-journal-new-entry))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package run-assoc
  :config
  (global-set-key (kbd "C-x C-r") 'run-associated-program)
  (setq associated-program-alist
        '(("mupdf" "\\.pdf")
          ("mpv" "\\.mkv")
          ("mpv" "\\.mp4")
          ("waterfox-current" "\\.html")
          ("libreoffice" "\\.docx"))))

(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

(use-package smart-tabs-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (smart-tabs-insinuate 'c))

(advice-add 'yank :after
            (lambda (ARG)
              "Indent the text just yanked."
              (indent-region (region-beginning) (region-end))))

(advice-add 'yank-pop :after
            (lambda (ARG)
              "Indent the text just popped from the kill ring."
              (indent-region (region-beginning) (region-end))))

(setq tramp-default-method "ssh")

(use-package visible-mark
  :ensure t
  :config
  (setq visible-mark-max 3)
  (global-visible-mark-mode)
  :bind
  ("C--" . avy-pop-mark))

(use-package which-key
  :init
  (which-key-mode))

(use-package ws-butler
  :hook
  (prog-mode-hook . ws-butler-mode))

(use-package yasnippet
  :config (yas-global-mode 1))
