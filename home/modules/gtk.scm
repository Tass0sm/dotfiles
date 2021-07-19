(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files))

(define-public gtk-config-service
  (list
   (simple-service 'gtk-config
 		   home-files-service-type
 		   `(("config/gtk-3.0/settings.ini"
                      ,(local-file "../files/settings.ini"))))))
