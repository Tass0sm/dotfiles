(define-module (home modules herd)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd))

(define-public herd-services
  (list
   (service home-shepherd-service-type
	    (home-shepherd-configuration
             (auto-start? #f)))))
