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
  #:use-module (home modules shell)
  #:use-module (home modules emacs)
  #:use-module (home modules clojure)
  #:use-module (home modules guile))

(define base-packages
  (map specification->package
       (list
	;; terminal emulator + ???
	"glibc-locales")))

(home-environment
  (packages
   `(,@base-packages
     ;; Development
     ,@clojure-packages
     ,@guile-packages))
  (services
   `(,@emacs-services
     ,@zsh-services)))
