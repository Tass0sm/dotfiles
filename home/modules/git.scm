(define-module (home modules git)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control))

(define-public git-packages
  (list `(,git "send-email")
        `(,git "credential-libsecret")
        git-lfs))

(define-public git-services
  (list
   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
		 ((name . "Tassos Manganaris")
                  (email . "tassos.manganaris@gmail.com")))
	        (github
		 ((user . "Tass0sm")))
                (sendemail
                 ((smtpServer . "smtp.gmail.com")
                  (smtpServerPort . "587")
                  (smtpencryption . "tls")
                  (smtpuser . "tassos.manganaris@gmail.com")))
	        (credential
		 ((helper . ,#~(string-append #$git:credential-libsecret "/bin/git-credential-libsecret"))))))
             (ignore
 	      '(".envrc"))))))
