;; Load path Config

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Custom

(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file)

;; Visual Customization

(load (concat user-emacs-directory "visual.el"))
(setq-default truncate-lines t)

;; BSPWM Ease of life

(load (concat user-emacs-directory "bspwm-integration.el"))

(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/elpa/use-package-20200322.2110")
  (require 'use-package))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package projectile
  :init
  (projectile-mode +1))

(use-package which-key
  :init
  (which-key-mode))

(setq backup-directory-alist
 '(("." . "~/.local/share/emacs/saves/")))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package avy
  :ensure t)

(use-package ryo-modal
  :commands ryo-modal-mone
  :chords ("jk" . ryo-modal-mode)
  :bind ("C-c SPC" . ryo-modal-mode)
  :config
  (ryo-modal-keys
   ("q" ryo-modal-mode)
   ("s" isearch-forward)
   ("a" avy-goto-char-timer)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ("i" previous-line)
   ("x" execute-extended-command)
   ("v" scroll-up-command)
   ("u" scroll-down-command)
   ("c" recenter-top-bottom))
  (ryo-modal-keys
   ("p"
    (("p" projectile-switch-project :name "Switch Project")
     ("f" projectile-find-file)))
   ("f"
    (("f" find-file :name "Find File")
     ("s" save-buffer :name "Save Buffer")
     ("k" kill-buffer :name "Kill Buffer")))
   ("o"
    (("a" org-agenda-list :name "Org Agenda List")))
   ("b"
    (("s" ivy-switch-buffer)))
   ("w"
    (("1" delete-other-windows))))
  (ryo-modal-mode 1))

(use-package ivy
  :config (ivy-mode 1))

;; Yasnippet

(use-package yasnippet
  :config (yas-global-mode 1))

;; Smooth Scroll

;; (use-package smooth-scroll
;;   :config (smooth-scroll-mode t))

;; Org

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
  (setq org-journal-dir "~/Org/diary"))

(setq org-agenda-files '("~/Org/school.org"
                         "~/Org/projects.org"))
;			 "~/Org/life.org"
;			 "~/Org/routine.org"))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "KILL" "FAIL")))

;; Octave

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; SLIME

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup)

;; WS-BUTLER

(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))
(put 'downcase-region 'disabled nil)

;; Dired Hide Details

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-auto-revert-buffer t
        dired-dwim-target t))

;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; Smart Tabs

(use-package smart-tabs-mode
  :config

 (smart-tabs-add-language-support lisp lisp-mode-hook
   ((lisp-indent-line . lisp-indent-offset)
    (lisp-indent-region . lisp-indent-offset)))

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (smart-tabs-insinuate 'c 'lisp))

(advice-add 'yank :after
            (lambda (ARG)
              "Indent the text just yanked."
              (indent-region (region-beginning) (region-end))))

(advice-add 'yank-pop :after
            (lambda (ARG)
              "Indent the text just popped from the kill ring."
              (indent-region (region-beginning) (region-end))))

;; Flyspell

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode))

;; Magit

(use-package magit
  :ensure t
  :bind
  ("\C-x g" . magit-status))

;; AucTEX

(with-eval-after-load "tex"
  (add-to-list 'TeX-view-program-list '("mupdf" "/usr/bin/mupdf %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf")))

;; to use pdfview with auctex
;;(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;;
;; to have the buffer refresh after compilation
;;(add-hook 'TeX-after-compilation-finished-functions
;;          #'TeX-revert-document-buffer)
;;(put 'LaTeX-narrow-to-environment 'disabled nil)

;; RunAssoc

(use-package run-assoc
  :config
  (setq associated-program-alist
        '(("mupdf" "\\.pdf")
          ("waterfox-current" "\\.html"))))

;; Expand Region

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; Column Number Mode

(column-number-mode)

;; Workspaces / Perspectives

;; (use-package persp-mode
;;   :ensure t
;;   :config
;;   (persp-mode 1))
