(define-module (home modules desktop)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu home-services xorg)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (tassos-guix packages xorg)
  #:use-module (tassos-guix home-services wm)
  #:use-module (tassos-guix home-services notifications)
  ;; from rde
  #:use-module (rde gexp)
  #:use-module (gnu home-services-utils))

(define-public shallow-nordic-theme
  (package
    (inherit nordic-theme)
    (name "shallow-nordic-theme")
    (arguments
     `(#:install-plan
       `(("." "."
          #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                     "package-lock.json" "Gulpfile.js")))))))

(define-public desktop-packages
  (map specification->package
       (list
        "sx"
        "cava"
        "light"
        "dmenu"
        "bemenu"
        "polybar"
        "flameshot"
        "xsel"
        "wl-clipboard"
        "xwallpaper"
        "nordic-theme"
	"xcursor-nordzy")))

(define-public desktop-services
  (list
   (simple-service 'sx-config
        	   home-files-service-type
        	   `((".config/sx/sxrc"
                      ,(mixed-executable-text-file
                        "sxrc"
                        (slurp-file-like (local-file "../files/sxrc"))))))
   (simple-service 'nord-xresources-file
        	   home-files-service-type
        	   `((".config/nord-xresources"
                      ,(local-file "../files/nord-xresources"))))
   (service home-xresources-service-type
            (home-xresources-configuration
             (config
              `((include . ,(string-append "\"" (getenv "HOME") "/.config/nord-xresources\""))))))
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
   (simple-service 'polybar-scripts-config
        	   home-files-service-type
        	   `((".config/polybar/mic-tog.sh"
                      ,(local-file "../files/polybar/mic-tog.sh" #:recursive? #t))))
   (simple-service 'gtk-config
        	   home-files-service-type
        	   `((".config/gtk-3.0/settings.ini"
                      ,(local-file "../files/gtk3.ini"))
        	     (".config/gtk-3.0/gtk.css"
                      ,(local-file "../files/gtk3.css"))))
   (simple-service 'wallpaper-file
        	   home-files-service-type
        	   `(("pictures/wallpaper.png"
                      ,(local-file "../files/images/genome-wallpaper.png"))))
   (simple-service 'gtk-theme
        	   home-files-service-type
        	   `((".local/share/themes/Nordic"
                      ,shallow-nordic-theme)))))
