(define-module (home modules git)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control))

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
		(credential
		 ((helper . "/usr/share/git/credential/libsecret/git-credential-libsecret")))))
             (ignore
 	      '(".envrc"))))))
