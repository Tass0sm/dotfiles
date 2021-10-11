(define-module (home modules mail)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages mail)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  ;; from rde
  #:use-module (gnu home-services mail))

;; Plan: Thunderbird as Mail Retrieval Agent. Use notmuch as Mail User
;; Agent. Make reproducible config here.

(define-public mail-packages
  (map specification->package
       '()))

(define-public mail-services
  (list
   (service home-notmuch-service-type
	    (home-notmuch-configuration
             (config
              `((user
                 ((name . "Anastasios Manganaris")
                  (primary_email . "tassos.manganaris@gmail.com")))
                (database
                 ((path . "/home/tassos/.thunderbird/paws4vo9.default-release/ImapMail")))
                (new
                 ())))))))
