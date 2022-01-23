(define-module (home modules desktop)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services xorg)

  #:use-module (guix gexp)
  #:use-module (tassos-guix packages xorg)
  #:use-module (tassos-guix home-services wm)
  #:use-module (tassos-guix home-services notifications))

(define-public desktop-packages
  (map specification->package
       (list
        "polybar"
        "xwallpaper"
	"xcursor-nordzy")))

(define-public desktop-services
  (list
   (service home-xresources-service-type
	    (home-xresources-configuration
	     (config
	      `((include . "\"/home/tassos/.config/guix/home/files/nord-xresources\"")))))
   (service home-dunst-service-type
            (home-dunst-configuration
             (dunstrc (list
        	       (local-file "../files/dunstrc")))))
   (service home-sxhkd-service-type
            (home-sxhkd-configuration
             (sxhkdrc (list
        	       (local-file "../files/sxhkdrc")))))
   (service home-bspwm-service-type
            (home-bspwm-configuration
             (bspwmrc (list
        	       (local-file "../files/bspwmrc")))))
   (service home-polybar-service-type
	    (home-polybar-configuration
	     (config (list
		      (local-file "../files/polybar.ini")))))
   (simple-service 'gtk-config
        	   home-files-service-type
        	   `(("config/gtk-3.0/settings.ini"
                      ,(local-file "../files/gtk3.ini"))
        	     ("config/gtk-3.0/gtk.css"
                      ,(local-file "../files/gtk3.css"))))))
