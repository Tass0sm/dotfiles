;; Configure Load Path

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "elpa/use-package-20200322.2110"))

;; Load Use-Package

(require 'use-package)

;; Add Chords to Use-Package

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; Custom File

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

;; Window Manager Specific Config

(defun config-for-bspwm ()
  "Configure emacs for bspwm."
  (org-babel-load-file (concat user-emacs-directory "bspwm-integration.org"))
  (org-babel-load-file (concat user-emacs-directory "visual.org")))

(defun config-for-exwm ()
  "Configure emacs for exwm."
  (org-babel-load-file (concat user-emacs-directory "exwm.org")))

(let ((wm (getenv "WM")))
  (cond
   ((string= wm "bspwm") (config-for-bspwm))
   (t (config-for-exwm))))

;; Undisabled Commands

(put 'scroll-left 'disabled nil)

;; Normal Files

(org-babel-load-file (concat user-emacs-directory "main.org"))
