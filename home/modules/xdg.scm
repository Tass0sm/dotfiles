(define-module (home modules xdg)
  #:use-module (gnu packages)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg))

(define-public xdg-packages
  (map specification->package
       (list
        "xdg-utils")))

(define-public xdg-services
  (list
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (added '(("application/pdf" . "firefox.desktop")
                      ("application/pdf" . "emacsclient.desktop")
                      ("inode/directory" . "emacsclient.desktop")))
             (default '(("x-scheme-handler/http" . "firefox.desktop")
                        ("x-scheme-handler/https" . "firefox.desktop")
                        ("x-scheme-handler/mailto" . "userapp-Evolution-7C6SL1.desktop")
                        ("application/pdf" . "emacsclient.desktop")
                        ("text/plain" . "emacsclient.desktop")
                        ("image/jpeg" . "sxiv.desktop")
                        ("image/png" . "sxiv.desktop")))
             (removed '())
             (desktop-entries '())))
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (desktop     "$HOME/desktop")
             (documents   "$HOME/documents")
             (download    "$HOME/downloads")
             (music       "$HOME/music")
             (pictures    "$HOME/pictures")
             (publicshare "$HOME/public")
             (templates   "$HOME/templates")
             (videos      "$HOME/videos")))))
