(define-module (home modules gtk)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services))

(define-public gtk-services
  (list
   (simple-service 'gtk-config
 		   home-files-service-type
 		   `(("config/gtk-3.0/settings.ini"
                      ,(local-file "../files/gtk3.ini"))
		     ("config/gtk-3.0/gtk.css"
                      ,(local-file "../files/gtk3.css"))))))
