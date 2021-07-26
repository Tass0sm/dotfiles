(define-module (home modules desktop)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)

  #:use-module (tassos-guix home-services desktop))

(define-public desktop-services
  (list
   (service home-sxhkd-service-type
	    (home-sxhkd-configuration))
   (service home-bspwm-service-type
	    (home-bspwm-configuration
	     (bspwmrc '("#test"))))))
