(define-module (home modules desktop)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services xorg)

  #:use-module (guix gexp)
  #:use-module (tassos-guix home-services wm)
  #:use-module (tassos-guix home-services notifications))

(define-public desktop-services
  (list
   (service home-xresources-service-type
	    (home-xresources-configuration
	     (config
	      `((include . "\"/home/tassos/.config/guix/home/files/nord-xresources\"")))))
   (service home-dunst-service-type
	    (home-dunst-configuration
	     (dunstrc (list
		       (slurp-file-gexp
			(local-file "../files/dunstrc"))))))
   ;; (service home-polybar-service-type
   ;; 	    (home-polybar-configuration
   ;; 	     (config (list
   ;; 		      (slurp-file-gexp
   ;; 		       (local-file "../files/polybar-config"))))))
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
