(require 'package)

(setq
 default-directory
 "~/Desktop")
(setq
 backup-directory-alist
 '(("." . "~/.saves/")))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(load-theme 'deeper-blue)

(package-initialize)

(setq python-shell-completion-native-enable nil)

(require 'org)
(setq org-agenda-files (list
 			"~/Desktop/organization/school.org"
 			"~/Desktop/organization/career/career.org"
 			"~/Desktop/organization/environment.org"
 			"~/Desktop/organization/life.org"
 			"~/Desktop/organization/fitness/fitness.org"
 			"~/Desktop/organization/projects.org"))

(setq org-directory "~/Desktop/organization/")
(setq org-log-done t)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keyword-faces
      '(("FAIL" . "yellow")))

(require 'yasnippet)
(yas-global-mode t)

(require 'org-journal)
(setq org-journal-dir "~/Desktop/organization/diary/")

(global-set-key (kbd "C-x g") 'magit-status)

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(setq-default truncate-lines t)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-buffer)

(require 'meghanada)

(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            ;;(add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
 	    ))
(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
  (setq meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn")))

(put 'scroll-left 'disabled nil)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

(require 'rust-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
;(setq TeX-PDF-mode t)

(require 'flymake)

(defun flymake-get-tex-args (file-name)
  "??"
  (list "pdflatex"
	(list "-file-line-error" "-draftmode" "-interaction=nonstopmode"
	      file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(require 'mental-math-mode "~/.dotfiles/emacs.d/mental-math/mental-math-mode.el")
