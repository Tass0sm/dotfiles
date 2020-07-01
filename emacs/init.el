(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file)

;; Visual Customization

(load (concat user-emacs-directory "visual.el"))

;; BSPWM Ease of life

(load (concat user-emacs-directory "bspwm-integration.el"))

(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/elpa/use-package-20200322.2110")
  (require 'use-package))

(use-package package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package projectile)
(projectile-mode +1)

(use-package which-key)
(which-key-mode)

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
  (ryo-modal-mode 1))

(ryo-modal-keys
 ("p"
  (("p" projectile-switch-project :name "Switch Project")
   ("f" projectile-find-file)))
 ("f"
  (("f" find-file :name "Find File")
   ("s" save-buffer :name "Save Buffer")
   ("k" kill-buffer :name "Kill Buffer")))
 ("o"
  (("a" org-agenda :name "Org Agenda")))
 ("b"
  (("s" ivy-switch-buffer)))
 ("w"
  (("1" delete-other-windows))))

;; (ryo-modal-major-mode-keys
;;  'org-agenda
;;  ("t" org-agenda-todo))


(use-package ivy
  :config (ivy-mode 1))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package smooth-scroll
  :config (smooth-scroll-mode t))

(setq org-agenda-files '("~/Org/school.org"
			 "~/Org/projects.org"
			 "~/Org/life.org"
			 "~/Org/routine.org"))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "KILL" "FAIL")))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Org/diary"))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
