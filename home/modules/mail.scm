(define-module (home modules mail)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  ;; from rde
  #:use-module (gnu home-services mail)
  ;; custom
  #:use-module (tassos-guix packages mail))

(define-public mail-packages
  (list
   evolution
   evolution-data-server
   evolution-ews
   evolution-on))

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
