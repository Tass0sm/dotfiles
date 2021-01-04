;; Set up package.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Configure Load Path

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Load Use-Package

(require 'use-package)

;; Add Chords to Use-Package

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; Custom File

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Window Manager Specific Config

(defun tassos/config-for-bspwm ()
  "Configure emacs for bspwm."
  (org-babel-load-file (concat user-emacs-directory "bspwm-integration.org"))
  (org-babel-load-file (concat user-emacs-directory "visual.org")))

(defun tassos/config-for-exwm ()
  "Configure emacs for exwm."
  (load-theme 'night-owl t)
  (org-babel-load-file (concat user-emacs-directory "exwm.org")))

(let ((wm (getenv "WM")))
  (cond
   ((string= wm "bspwm") (tassos/config-for-bspwm))
   (t (tassos/config-for-exwm))))

;; Undisabled Commands

(put 'scroll-left 'disabled nil)

;; Normal Files

(org-babel-load-file (concat user-emacs-directory "main.org"))
