;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home environment)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu home-services)
  ;; personal modules
  #:use-module (home modules xdg)
  #:use-module (home modules xdg)
  #:use-module (home modules shell)
  #:use-module (home modules emacs)
  #:use-module (home modules git)
  #:use-module (home modules flameshot))

(define base-packages
  (map specification->package
       (list
	;; terminal emulator + ???
	"nss-certs")))

(home-environment
  (packages
   `(,@base-packages
     ;; Development
     ))
  (services
   `(,@xdg-services
     ,@zsh-services
     ,@git-services
     ,@emacs-services
     ,@flameshot-services)))
