(define-module (home modules flameshot)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages image)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils))

					; Flameshot

;; the boolean for server-mode? isn't serialized
(define (serialize-boolean field-name val) "")

(define-configuration home-flameshot-configuration
  (package
   (package flameshot)
   "Flameshot package to use.")
  (server-mode?
   (boolean #f)
   "Create a shepherd service, which starts a flameshot deamon."))

(define (add-flameshot-package config)
  (list (home-flameshot-configuration-package config)))

(define (add-flameshot-shepherd-service config)
  (optional (home-flameshot-configuration-server-mode? config)
	    (list (shepherd-service
		   (documentation "Flameshot daemon.")
		   (provision '(flameshot-daemon))
		   (start #~(make-forkexec-constructor
			     (list #$(file-append
				      (home-flameshot-configuration-package config)
				      "/bin/flameshot"))
			     #:log-file (string-append
					 (or (getenv "XDG_LOG_HOME")
					     (format #f "~a/.local/var/log"
						     (getenv "HOME")))
					 "/flameshot.log")))
		   (stop #~(make-kill-destructor))))))

(define home-flameshot-service-type
  (service-type (name 'home-flameshot)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-flameshot-package)
		       (service-extension
			home-shepherd-service-type
			add-flameshot-shepherd-service)
		       ;; (service-extension
                       ;;  home-files-service-type
                       ;;  add-flameshot-configuration)
		       ))
                (default-value (home-flameshot-configuration))
                (description "Install and configure flameshot.")))

(define-public flameshot-services
  (list
   (service home-flameshot-service-type
	    (home-flameshot-configuration
	     (server-mode? #t)))))
