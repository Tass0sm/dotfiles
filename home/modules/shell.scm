(define-module (home modules shell)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
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
	      '(("EDITOR" . "\"emacsclient -a ''\"")
		("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
		("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
		("SSL_CERT_FILE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
		("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")))
	     (zshrc
	      (list (slurp-file-gexp (local-file "../files/zshrc"))))
	     (zprofile
	      (list (slurp-file-gexp (local-file "../files/zprofile"))))))))
