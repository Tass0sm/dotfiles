;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services symlink-manager)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  ;; custom services
  #:use-module ((tassos-guix home-services shells) #:prefix my:)
  ;; personal modules
  #:use-module (home modules xdg)
  #:use-module (home modules git)
  #:use-module (home modules mail)
  #:use-module (home modules herd)
  #:use-module (home modules emacs)
  #:use-module (home modules shell)
  #:use-module (home modules desktop))

(define base-packages
  (map specification->package
       (list
	"alacritty"
	"exa"
        "pamixer"
	"hunspell"
	"hunspell-dict-en"
        "poweralertd"
        "curl"
        "ripgrep"
        "inetutils"
	"nss-certs"
        "glibc-locales")))

(define font-packages
  (map specification->package
       (list
	"font-iosevka"
        "font-awesome")))

(define home-scripts
  (package
   (name "home-scripts")
   (version "0.1")
   (source (local-file "files/scripts"
                       #:recursive? #t))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      '(("ec.sh" "bin/ec")
        ("term.sh" "bin/term")
        ("todos.sh" "bin/todos")
        ("reconfigure.sh" "bin/reconfigure"))))
   (propagated-inputs
    `(("epipe" ,epipe)))
   (home-page "https://github.com/Tass0sm/dotfiles")
   (synopsis "My personal scripts.")
   (description "My personal scripts.")
   (license license:expat)))

(define my-packages
  `(,@base-packages
    ,@xdg-packages
    ,@zsh-packages
    ,@git-packages
    ,@desktop-packages
    ,@font-packages
    ,home-scripts))

(home-environment
 (packages my-packages)
 (essential-services
  (list
   (service home-run-on-first-login-service-type)
   (service home-activation-service-type)
   (service home-environment-variables-service-type)

   (service home-symlink-manager-service-type)

   (service home-fontconfig-service-type)
   (service home-xdg-base-directories-service-type)
   (service my:home-shell-profile-service-type)

   (service home-service-type)
   (service home-profile-service-type my-packages)))
 (services
  `(,@xdg-services
    ,@zsh-services
    ,@git-services
    ,@herd-services
    ,@emacs-services
    ,@desktop-services)))
