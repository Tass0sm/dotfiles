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
       '("emacs-ivy"
	 "emacs-which-key"
	 "emacs-use-package"
	 "emacs-sudo-edit"

	 "emacs-multiple-cursors"

	 "emacs-org-journal"
	 
	 "emacs-dashboard"
	 "emacs-magit"
	 "emacs-guix"
	 "emacs-projectile"
	 "emacs-company")))

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
