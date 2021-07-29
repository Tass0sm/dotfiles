(define-module (home modules desktop)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)

  #:use-module (guix gexp)
  #:use-module (tassos-guix home-services desktop))

(define-public desktop-services
  (list
   (service home-sxhkd-service-type
	    (home-sxhkd-configuration
	     (sxhkdrc (list
		       (slurp-file-gexp
			(local-file "../files/sxhkdrc"))))))
   (service home-bspwm-service-type
	    (home-bspwm-configuration
	     (bspwmrc (list
		       (slurp-file-gexp
			(local-file "../files/bspwmrc"))))))))
