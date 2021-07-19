(define-module (home modules guile)
  #:use-module (gnu packages))

(define-public guile-packages
  (map specification->package
       (list
	"guile"
	"guile-readline"
	"guile-colorized")))
