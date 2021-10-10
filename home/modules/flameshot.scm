(define-module (home modules flameshot)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages image)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)

  #:use-module (tassos-guix home-services flameshot))

(define-public flameshot-services
  (list
   (service home-flameshot-service-type
	    (home-flameshot-configuration
	     (server-mode? #t)))))
