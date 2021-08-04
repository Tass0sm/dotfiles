(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services emacs))

(define emacs-packages
  (map specification->package
       '(;; Utility
	 "emacs-ivy"
	 "emacs-which-key"
	 "emacs-use-package"
	 "emacs-sudo-edit"
	 "emacs-magit"
	 "emacs-guix"
	 "emacs-projectile"
	 "emacs-company"
	 ;; Modes
	 "emacs-yaml-mode"
	 ;; Racket
	 "emacs-racket-mode"
	 "emacs-geiser-racket"
	 ;; Haskell
	 "emacs-haskell-mode"
	 "emacs-dante"
	 ;; Clojure
	 "emacs-clojure-mode"
	 "emacs-cider"
	 ;; Text Editing
	 "emacs-expand-region"
	 "emacs-multiple-cursors"
	 "emacs-ws-butler"
	 "emacs-yasnippet"
	 "emacs-yasnippet-snippets"
	 ;; Org
	 "emacs-org-journal"
	 ;; Appearance
	 "emacs-nord-theme")))

(define-public emacs-services
  (list
   (simple-service 'emacs-init
 		   home-files-service-type
 		   `(("config/emacs/early-init.el"
                      ,(local-file "../files/early-init.el"))
		     ("config/emacs/init.el"
                      ,(local-file "../files/init.el"))))
   (service home-emacs-service-type
	    (home-emacs-configuration
             (package emacs-next-pgtk)
             ;; (rebuild-elisp-packages? #t)
             ;; (server-mode? #t)
             (elisp-packages emacs-packages)))))
