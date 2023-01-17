(define-module (home modules secrets)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu home-services xorg)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; from rde
  #:use-module (rde gexp)
  #:use-module (gnu home-services-utils)
  ;; custom services
  #:use-module (tassos-guix packages xorg)
  #:use-module (tassos-guix home-services wm)
  #:use-module (tassos-guix home-services notifications)
  #:use-module ((tassos-guix home-services shells) #:prefix my:))

(define-public secrets-packages
  (map specification->package
       (list
        "seahorse"
        "gnome-keyring")))

(define-public secrets-services
  (list
   (simple-service 'keyring-setup
                   my:home-shell-profile-service-type
                   (list
                    (plain-file "keyring-setup"
                                "eval $(gnome-keyring-daemon --daemonize --components=secrets,ssh)")))))
