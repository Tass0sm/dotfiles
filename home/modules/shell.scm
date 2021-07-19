(define-module (home modules shell)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services files))

 (define-public zsh-services
   (list
    (service home-zsh-autosuggestions-service-type)
    (service home-zsh-service-type
	     (home-zsh-configuration
	      (xdg-flavor? #t)
	      (environment-variables
	       '(("XDG_DATA_DIRS" . "${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}")
		 ("XDG_CONFIG_DIRS" . "${XDG_CONFIG_DIRS:-/etc/xdg/}")
		 ("EDITOR" . "\"emacsclient -a ''\"")))
	      (zlogin
	       '("GUIX_PROFILE=\"$HOME/.guix-profile\""
		 "[ -f \"$GUIX_PROFILE/etc/profile\" ] && . \"$GUIX_PROFILE/etc/profile\""))))))
