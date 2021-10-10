(define-module (home modules xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg))

(define-public xdg-services
  (list
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
