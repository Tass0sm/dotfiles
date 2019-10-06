(require 'package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-todo-keywords (quote ((sequence "TODO" "|" "FAIL" "DONE"))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (org-journal meghanada auctex dumb-jump multiple-cursors yoshi-theme pdf-tools haskell-mode org-plus-contrib company-irony gnuplot company-irony-c-headers yasnippet-snippets yasnippet auto-complete magit elpy w3 web-mode speed-type midi-kbd ess))))

(setq
 default-directory
 "~/Desktop")
(setq
 backup-directory-alist
 '(("." . "~/.saves/")))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(load-theme 'deeper-blue)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(setq python-shell-completion-native-enable nil)

(require 'org)
(setq org-agenda-files (list
			"~/Desktop/organization/school.org"
			"~/Desktop/organization/environment.org"
			"~/Desktop/organization/life.org"
			"~/Desktop/organization/fitness/fitness.org"
			"~/Desktop/organization/projects.org"))

(setq org-directory "~/Desktop/organization/")
;;(setq org-mobile-checksum-binary "c:/Users/tasso/Desktop/Emacs/Sha1sum/sha1sum.exe")
;;(setq org-mobile-inbox-for-pull "c:/Users/tasso/Desktop/Organization/flagged.org")
;;(setq org-mobile-directory "c:/Users/tasso/Dropbox/Apps/Orgzly")
(setq org-log-done t)
(define-key global-map "\C-ca" 'org-agenda)
;;(define-key global-map "\C-cl" 'org-store-link)

(setq org-todo-keyword-faces
      '(("FAIL" . "yellow")))

;;(require 'company)
;;(require 'company-irony)

;;(require 'company-irony-c-headers)

(add-hook 'after-init-hook 'global-company-mode)

(setq-default truncate-lines t)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony)))

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers)))

;;(require 'yasnippet)
;;(yas-global-mode t)

(global-set-key (kbd "C-x g") 'magit-status)

;;(require 'gnuplot)

;;(setq gnuplot-program "C:/Program Files (x86)/gnuplot/bin/gnuplot.exe")

;;(setq-default ispell-program-name "C:/msys64/mingw64/bin/aspell.exe")

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))

(setq meghanada-code-beautify-before-save nil)
(put 'scroll-left 'disabled nil)

(require 'org-journal)

(setq org-journal-dir "~/Desktop/organization/diary/")
